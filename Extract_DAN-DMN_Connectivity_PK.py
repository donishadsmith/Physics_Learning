
import pandas as pd
import numpy as np
import bids
#Extracting DAN-DMN connectivity values for the PK task.

deriv_dir = '/home/data/nbc/Laird_PhysicsLearning/dset-v2.0.0/derivatives'
#Link to text file containing all 400 nodes. 
labels = pd.read_csv('/home/dsmit216/schaefer400.txt', sep='\t', header=0, index_col=0)
dorsAttn = labels[labels['Network'] == 'DorsAttn'].index
default = labels[labels['Network'] == 'Default'].index
labels.rename({'Unnamed: 2': '#'}, axis=1, inplace=True)

#Dmnine task, session, and file location for IDConn.
sessions = [1,2]
task = 'reas'
conditions = ['Reasoning', 'Baseline']

layout = bids.BIDSLayout('/home/data/nbc/Laird_PhysicsLearning/dset-v2.0.0', derivatives=True, database_path='/home/data/nbc/Laird_PhysicsLearning/dset-v2.0.0dset-BIDSLayout.db_cache')
subjects = layout.get(return_type='id', target='subject', suffix='bold')

index = pd.MultiIndex.from_product([subjects, sessions, conditions])
out_file = pd.DataFrame(index=index, columns=['DorsAttn.Default.conn'])

columns = pd.MultiIndex.from_product([dorsAttn, default])
dan_dmn_node_conn = pd.DataFrame(index=index, columns=columns)

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
                #Calculating mean of all nodes for the DAN and DMN for the between-network connectivity measure.
                graph.columns = graph.columns.astype(int)
                dan_dmn_graph = graph.loc[default][dorsAttn]
                out_file.at[(subject, session, condition), 'DorsAttn.Default.conn'] = np.mean(dan_dmn_graph.mean())
                out_file.to_csv(f'{deriv_dir}/idconn-v0.1-presub+90.g6973d15/reas_dan-dmn_network-connectivity.tsv', sep='\t')
                #Obtaining connectivity values for all nodes in DAN and DMN to create average within-network connectivity values using R script.
                all = list(dorsAttn) + list(dmnault)
                dan_dmn_graph = graph.loc[all][all]
                for i in dan_dmn_graph.index:
                    for j in dan_dmn_graph.columns:
                        dan_dmn_node_conn.at[(subject, session, condition), (i,j)] = dan_dmn_graph.loc[i,j]
                dan_dmn_node_conn.to_csv(f'{deriv_dir}/idconn-v0.1-presub+90.g6973d15/reas_dan-dmn_node-connectivity.tsv', sep='\t')

            except Exception as e:
                print(f'Subject {subject}, session {session}, REAS-{condition} didn\'t run because: {e}')

out_file.to_csv(f'{deriv_dir}/idconn-v0.1-presub+90.g6973d15/dan-dmn_connectivity.tsv', sep='\t')

