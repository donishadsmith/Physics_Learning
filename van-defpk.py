import pandas as pd
import numpy as np
import bids


deriv_dir = '/home/data/nbc/Laird_PhysicsLearning/dset-v2.0.0/derivatives'

labels = pd.read_csv('/home/dsmit216/schaefer400.txt', sep='\t', header=0, index_col=0)
ventAttn = labels[labels['Network'] == 'SalVentAttn'].index
default = labels[labels['Network'] == 'Default'].index
labels.rename({'Unnamed: 2': '#'}, axis=1, inplace=True)

sessions = [1,2]
task = 'reas'
conditions = ['Reasoning', 'Baseline']

layout = bids.BIDSLayout('/home/data/nbc/Laird_PhysicsLearning/dset-v2.0.0', derivatives=True, database_path='/home/data/nbc/Laird_PhysicsLearning/dset-v2.0.0dset-BIDSLayout.db_cache')
subjects = layout.get(return_type='id', target='subject', suffix='bold')

index = pd.MultiIndex.from_product([subjects, sessions, conditions])
out_file = pd.DataFrame(index=index, columns=['SalVenAttn.Default.conn'])

columns = pd.MultiIndex.from_product([ventAttn, default])
van_def_node_conn = pd.DataFrame(index=index, columns=columns)

for subject in subjects:
    print(subject)
    for session in sessions:
        print(session)
        for condition in conditions:
            print(condition)
            try:
                graph = pd.read_csv(f'{deriv_dir}/idconn-0+unknown_reas/sub-{subject}/ses-{session}/func/sub-{subject}_ses-{session}_task-reas_condition-{condition}_desc-Schaefer400_2mm_corrmat.tsv', 
                                    sep='\t', 
                                    header=0, 
                                    index_col=0)
                
                graph.columns = graph.columns.astype(int)
                van_def_graph = graph.loc[default][ventAttn]
                out_file.at[(subject, session, condition), 'SalVenAttn.Default.conn'] = np.mean(van_def_graph.mean())
                out_file.to_csv(f'{deriv_dir}/idconn-v0.1-presub+90.g6973d15/reas_van-def_network-connectivity.tsv', sep='\t')

                all = list(ventAttn) + list(default)
                van_def_graph = graph.loc[all][all]
                for i in van_def_graph.index:
                    for j in van_def_graph.columns:
                        van_def_node_conn.at[(subject, session, condition), (i,j)] = van_def_graph.loc[i,j]
                van_def_node_conn.to_csv(f'{deriv_dir}/idconn-v0.1-presub+90.g6973d15/reas_van-def_node-connectivity.tsv', sep='\t')

            except Exception as e:
                print(f'Subject {subject}, session {session}, REAS-{condition} didn\'t run because: {e}')

out_file.to_csv(f'{deriv_dir}/idconn-v0.1-presub+90.g6973d15/van-def_connectivity.tsv', sep='\t')
