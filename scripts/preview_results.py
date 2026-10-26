"""Inspect saved data and produce portfolio figures without retraining models."""
from pathlib import Path
import argparse
import json
import os
import tempfile

import numpy as np
import pandas as pd
os.environ.setdefault('MPLCONFIGDIR', str(Path(tempfile.gettempdir())/'nps-matplotlib'))
import matplotlib
matplotlib.use('Agg')
import matplotlib.pyplot as plt

ROOT = Path(__file__).resolve().parents[1]
BLUE, TEAL, GRAY = '#244b68', '#25857b', '#8b96a3'


def load_data():
    history = pd.read_csv(ROOT/'data/processed/pop1982-2024_adj.csv')
    forecasts = pd.read_csv(ROOT/'results/df_predicted_agaci.csv', low_memory=False)
    return history, forecasts


def audit_data(history, forecasts):
    return {
        'historical': {
            'rows': len(history),
            'municipal_identifiers': int(history.COD_COM.nunique()),
            'year_min': int(history.ANNO.min()),
            'year_max': int(history.ANNO.max()),
            'missing_by_column': {k: int(v) for k,v in history.isna().sum().items()},
            'duplicate_municipality_year_rows': int(history.duplicated(['COD_COM', 'ANNO']).sum()),
        },
        'saved_forecasts': {
            'rows': len(forecasts),
            'municipal_identifiers': int(forecasts.COD_COM.nunique()),
            'year_min': int(forecasts.ANNO.min()),
            'year_max': int(forecasts.ANNO.max()),
            'missing_by_column': {k: int(v) for k,v in forecasts.isna().sum().items()},
            'reversed_interval_bounds': int((forecasts.POP_LO > forecasts.POP_HI).sum()),
            'point_forecasts_outside_interval': int(((forecasts.POP_PRED < forecasts.POP_LO) |
                                                    (forecasts.POP_PRED > forecasts.POP_HI)).sum()),
        },
        'interpretation': [
            'Diagnostics describe the original saved files; records are not changed.',
            'Identifier gaps and repeated municipality/year pairs need source-level review.',
            'A point forecast outside a quantile interval is reported, not automatically treated as an invalid interval.',
            'Saved forecasts are not the held-out validation predictions.',
        ],
    }


def save_figures(history, forecasts, output):
    output.mkdir(parents=True, exist_ok=True)
    plt.rcParams.update({'font.family':'DejaVu Sans', 'font.size':10,
                         'axes.spines.top':False, 'axes.spines.right':False,
                         'axes.titleweight':'bold', 'figure.facecolor':'white'})
    labels = ['LSTM-based method', 'GAM', 'Naive baseline']
    fig, axes = plt.subplots(1, 2, figsize=(10, 4.3))
    for ax, values, title in zip(axes, [[256.148941,291.860200,322.538936],
                                       [1297.388405,1548.728208,3793.474218]],
                                 ['Mean absolute error', 'Root mean squared error']):
        bars=ax.barh(labels, values, color=[TEAL,BLUE,GRAY], height=0.55)
        ax.invert_yaxis()
        ax.set_title(title, loc='left', pad=14)
        ax.set_xlabel('People · lower is better')
        ax.set_xlim(0, max(values)*1.25)
        ax.bar_label(bars, fmt='%.1f', padding=5)
    fig.suptitle('Municipal population forecasting', x=0.03, ha='left', fontsize=17, fontweight='bold')
    fig.text(0.03,0.025,'Original saved real-data evaluation; models were not retrained for this figure.',fontsize=9,color='#525d68')
    fig.tight_layout(rect=(0,0.07,1,0.91))
    fig.savefig(output/'model_comparison.png',dpi=180)
    plt.close(fig)

    # Show a traceable small municipality, avoiding a misleading national total
    # across changing municipal codes and an incomplete geographic panel.
    common = sorted(set(history.COD_COM.dropna()) & set(forecasts.COD_COM))
    municipality = int(common[0])
    h=history[history.COD_COM==municipality].sort_values('ANNO')
    f=forecasts[forecasts.COD_COM==municipality].sort_values('ANNO')
    fig,ax=plt.subplots(figsize=(9,4.3))
    ax.plot(h.ANNO,h.POPOLAZIONE,color=BLUE,label='Historical observations')
    ax.plot(f.ANNO,f.POP_PRED,color=TEAL,label='Saved forecast')
    ax.fill_between(f.ANNO.to_numpy(),f.POP_LO.to_numpy(),f.POP_HI.to_numpy(),
                    color=TEAL,alpha=0.18,label='Saved prediction interval')
    ax.set(title=f'Example municipality · code {municipality}',xlabel='Year',ylabel='Population')
    ax.legend(frameon=False,loc='upper left')
    fig.text(0.08,0.02,'Illustrative saved output; interval calibration limitations are discussed in reports/RESULTS.md.',fontsize=9,color='#525d68')
    fig.tight_layout(rect=(0,0.06,1,1))
    fig.savefig(output/'example_forecast.png',dpi=180)
    plt.close(fig)


def main():
    parser=argparse.ArgumentParser(description=__doc__)
    parser.add_argument('--output-dir', type=Path, default=ROOT/'reports/figures')
    args=parser.parse_args()
    history, forecasts=load_data()
    report=audit_data(history,forecasts)
    (ROOT/'reports/data_quality.json').write_text(json.dumps(report,indent=2)+'\n',encoding='utf-8')
    save_figures(history,forecasts,args.output_dir)
    print(json.dumps(report,indent=2))
    print(f'Figures written to {args.output_dir}')


if __name__=='__main__':
    main()
