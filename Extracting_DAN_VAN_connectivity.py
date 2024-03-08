## Credit to Katie Bottenhorn for this script. Github: https://github.com/62442katieb
import pandas as pd
import numpy as np
import bids

#This script extracts connectivity values between all nodes within the DAN and VAN and averages the nodes between the DAN and VAN for the FCI task.
deriv_dir = ''
#Link to text file containing all 400 nodes. 
labels = pd.read_csv('', sep='\t', header=0, index_col=0)
dorsAttn = labels[labels['Network'] == 'DorsAttn'].index
ventAttn = labels[labels['Network'] == 'SalVentAttn'].index
labels.rename({'Unnamed: 2': '#'}, axis=1, inplace=True)
#Define task, session, and file location for IDConn.
sessions = [1,2]
task = 'fci'
conditions = ['physicsXscenario', 'physicsXquestion', 'physicsXanswers', 'nonphysicsXscenario', 'nonphysicsXquestion', 'nonphysicsXanswers']

layout = bids.BIDSLayout('', derivatives=True, database_path='')
subjects = layout.get(return_type='id', target='subject', suffix='bold')

index = pd.MultiIndex.from_product([subjects, sessions, conditions])
out_file = pd.DataFrame(index=index, columns=['DorsAttn.SalVenAttn.conn'])

columns = pd.MultiIndex.from_product([dorsAttn, ventAttn])
dan_van_node_conn = pd.DataFrame(index=index, columns=columns)

for subject in subjects:
    print(subject)
    for session in sessions:
        print(session)
        for condition in conditions:
            print(condition)
            try:
                graph = pd.read_csv(f'{deriv_dir}/idconn-0+unknown/sub-{subject}/ses-{session}/func/sub-{subject}_ses-{session}_task-fci_condition-{condition}_desc-Schaefer400_2mm_corrmat.tsv', 
                                    sep='\t', 
                                    header=0, 
                                    index_col=0)
                #Obtaining connectivity values for all nodes in DAN and VAN.
                graph.columns = graph.columns.astype(int)
                dan_van_graph = graph.loc[ventAttn][dorsAttn]
                out_file.at[(subject, session, condition), 'DorsAttn.SalVenAttn.conn'] = np.mean(dan_van_graph.mean())
                out_file.to_csv(f'{deriv_dir}/idconn-v0.1-presub+90.g6973d15/fci_dan-van_network-connectivity.tsv', sep='\t')
                 #Obtaining connectivity values for all nodes in DAN and VAN to create average within-network connectivity values using R script.
                all = list(dorsAttn) + list(ventAttn)
                dan_van_graph = graph.loc[all][all]
                for i in dan_van_graph.index:
                    for j in dan_van_graph.columns:
                        dan_van_node_conn.at[(subject, session, condition), (i,j)] = dan_van_graph.loc[i,j]
                dan_van_node_conn.to_csv(f'{deriv_dir}/idconn-v0.1-presub+90.g6973d15/fci_dan-van_node-connectivity.tsv', sep='\t')

            except Exception as e:
                print(f'Subject {subject}, session {session}, FCI-{condition} didn\'t run because: {e}')

out_file.to_csv(f'{deriv_dir}/idconn-v0.1-presub+90.g6973d15/dan-van_connectivity.tsv', sep='\t')
