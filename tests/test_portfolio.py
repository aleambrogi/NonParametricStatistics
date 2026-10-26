"""Check the packaged inputs, notebook entry points and a small model pass."""
from pathlib import Path
import ast
import importlib.util
import json
import unittest

import numpy as np
import pandas as pd

ROOT = Path(__file__).resolve().parents[1]


def notebook(name):
    return json.loads((ROOT/'notebooks'/name).read_text(encoding='utf-8'))


class PortfolioTests(unittest.TestCase):
    def test_historical_input_matches_model_schema(self):
        data=pd.read_csv(ROOT/'data/processed/pop1982-2024_adj.csv', nrows=1000)
        data=data.rename(columns={'COD_PROV':'COD_PRO'})
        self.assertTrue({'COD_COM','COD_PRO','COD_REG','ANNO','POPOLAZIONE'} <= set(data))
        nb=notebook('02_real_population_forecasting.ipynb')
        source='\n'.join(''.join(c['source']) for c in nb['cells'] if c['cell_type']=='code')
        self.assertIn('rename(columns={"COD_PROV": "COD_PRO"})', source)
        self.assertIn('ROOT / "data" / "processed"', source)

    def test_saved_predictions_have_ordered_finite_intervals(self):
        data=pd.read_csv(ROOT/'results/df_predicted_agaci.csv',low_memory=False)
        self.assertEqual((int(data.ANNO.min()),int(data.ANNO.max())),(2025,2040))
        self.assertFalse(data.duplicated(['COD_COM','ANNO']).any())
        self.assertTrue(np.isfinite(data[['POP_PRED','POP_LO','POP_HI']].to_numpy()).all())
        self.assertTrue((data.POP_LO <= data.POP_HI).all())

    def test_notebooks_compile_and_separate_generated_outputs(self):
        for path in (ROOT/'notebooks').glob('*.ipynb'):
            nb=json.loads(path.read_text(encoding='utf-8'))
            ids=[c['id'] for c in nb['cells']]
            self.assertEqual(len(ids),len(set(ids)))
            code='\n'.join(''.join(c['source']) for c in nb['cells'] if c['cell_type']=='code')
            ast.parse(code,filename=path.name)
            self.assertIn('RUN_DIR = ROOT / "results" / "runs"',code)
            self.assertNotIn('C:/Users/',code)

    @unittest.skipUnless(importlib.util.find_spec('torch'),'Install full requirements for the PyTorch smoke check')
    def test_lstm_forward_backward_on_synthetic_data(self):
        import torch
        nb=notebook('01_synthetic_population_forecasting.ipynb')
        wanted={'make_dummy_population_data','LSTMDataset','PopulationLSTM'}
        definitions=[]
        for c in nb['cells']:
            if c['cell_type']!='code': continue
            for node in ast.parse(''.join(c['source'])).body:
                if isinstance(node,(ast.FunctionDef,ast.ClassDef)) and node.name in wanted:
                    definitions.append(node)
        namespace={'np':np,'pd':pd,'torch':torch,'nn':torch.nn}
        exec(compile(ast.fix_missing_locations(ast.Module(body=definitions,type_ignores=[])),
                     '<original-model-definitions>','exec'),namespace)
        np.random.seed(42)
        torch.manual_seed(42)
        torch.set_num_threads(1)
        data=namespace['make_dummy_population_data'](n_regions=1,n_prov_per_reg=1,n_com_per_prov=3)
        data['PROV_ID']=0
        data['REG_ID']=0
        data['LOG_POP']=np.log(data.POPOLAZIONE)
        data['DLOG_POP']=data.groupby('COD_COM').LOG_POP.diff()
        data=data.dropna(subset=['DLOG_POP'])
        ds=namespace['LSTMDataset'](data,10,1982,2013)
        loader=torch.utils.data.DataLoader(ds,batch_size=8)
        model=namespace['PopulationLSTM'](1,1)
        x,y,p,r=next(iter(loader))
        predictions=model(x,p,r)
        loss=torch.nn.functional.mse_loss(predictions,y)
        loss.backward()
        self.assertEqual(predictions.shape,y.shape)
        self.assertTrue(torch.isfinite(loss).item())
        self.assertTrue(all(torch.isfinite(v.grad).all() for v in model.parameters() if v.grad is not None))


if __name__=='__main__':
    unittest.main()
