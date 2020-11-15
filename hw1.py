import http.client
import json
import time
import timeit
import sys
import collections
from pygexf.gexf import *


#
# implement your data retrieval code here
#

# API_KEY = sys.argv[1]
API_KEY = "b6067a13c48f7085dfebc09968a98643"

# complete auto grader functions for Q1.1.b,d
def min_parts():
    """
    Returns an integer value
    """
    # you must replace this with your own value
    return 1100

def lego_sets():
    """
    return a list of lego sets.
    this may be a list of any type of values
    but each value should represent one set

    e.g.,
    biggest_lego_sets = lego_sets()
    print(len(biggest_lego_sets))
    > 280
    e.g., len(my_sets)
    """

    # Make call to api
    connection = http.client.HTTPSConnection("rebrickable.com", port=None, timeout=60)
    headers = {
        "Accept": "application/json" 
    }

    connection.request("GET", "/api/v3/lego/sets/?page_size=290&min_parts={}&ordering=-num_parts&key={}".format(min_parts(), API_KEY), headers=headers)
    response = connection.getresponse()

    if response.status == 200:

        # Build set list from response
        #response = json.loads(response.read()).decode("windows-1252")
        decoder = json.JSONDecoder()
        response = decoder.decode(response.read().decode("windows-1252"))

        lego_sets = []
        for lego_set in response['results']: 
            lego_sets.append({'set_num': lego_set['set_num'], 'set_name': lego_set['name']})

        connection.close()
        return lego_sets[:2]  # Only keep 30 sets

    else:
        raise Exception ("HTTP response: {}".format(response.status))

#lego_sets()

def gexf_graph():
    """
    return the completed Gexf graph object

    For each set in lego_sets, get the top_20 parts. For each part:
    
    part color
    part quantity
    part name
    part number
    """

    gexf = Gexf("Yuanhao Wang","Lego Sets Visualization Graph")
    graph=gexf.addGraph("undirected","static","bricks graph")
    attr_id = graph.addNodeAttribute("Type", None, "string", force_id="node_type")

    with open('data.txt', 'a+') as outfile:


        top_lego_sets = lego_sets()

        last_run_time = float('-inf')

        for lego_set in top_lego_sets:

            # Check to see if we need to wait
            cur_run_start_time = time.time()
            if cur_run_start_time - last_run_time < 1:
                time.sleep(1)   # Sleep for 1 second
            
            last_run_time = cur_run_start_time
            
            # Add set as a node
            if not graph.nodeExists(lego_set['set_num']):
                node = graph.addNode(lego_set['set_num'], lego_set['set_name'], r='0', g='0', b='0')   #(id, label, r, g, b)
                node.addAttribute("node_type", "set")
                print("Added set: {}".format(lego_set['set_num']))
        
            # Get all parts for set
            connection = http.client.HTTPSConnection("rebrickable.com")
            headers = {
                "Accept": "application/json"
            }

            connection.request("GET", "/api/v3/lego/sets/{}/parts/?page_size=1000&key={}".format(lego_set['set_num'], API_KEY), headers=headers)
            response = connection.getresponse()
            
            decoder = json.JSONDecoder()
            response = decoder.decode(response.read().decode("windows-1252"))
            connection.close()

            # Get top 20 parts by quantity
            all_set_parts = []
            for part in response['results']: 
                all_set_parts.append({'quantity': int(part['quantity']), 'color': part['color']['rgb'], 'name': part['part']['name'], 'number': part['part']['part_num'], 'part_id': '{}_{}'.format(part['part']['part_num'], part['color']['rgb'])})
            
            all_set_parts.sort(key=lambda x: x['quantity'], reverse=True)
            all_set_parts = all_set_parts[:20]

            json.dump(all_set_parts, outfile)
            
            # Add each part on to the graph
            for part in all_set_parts:
                part_rgb = tuple(int(part['color'][i:i+2], 16) for i in (0, 2, 4))
                if not graph.nodeExists(part['part_id']):
                    node = graph.addNode(part['part_id'], part['name'], r=str(part_rgb[0]), g=str(part_rgb[1]), b=str(part_rgb[2]))
                    node.addAttribute("node_type", "part")
                    # Add edge from part to set 
                graph.addEdge('{}_{}'.format(part['part_id'], lego_set['set_num']) ,lego_set['set_num'], part['part_id'], part['quantity'])  #(id, source, target, weight="")
                print("Added part: {} of set: {}".format(part['part_id'], lego_set['set_num']))

    output_file=open("2_test.gexf","wb")
    gexf.write(output_file)

gexf_graph() 

# complete auto-grader functions for Q1.2.d

def avg_node_degree():
    """
    hardcode and return the average node degree
    (run the function called “Average Degree”) within Gephi
    """
    # you must replace this value with the avg node degree
    return -1

def graph_diameter():
    """
    hardcode and return the diameter of the graph
    (run the function called “Network Diameter”) within Gephi
    """
    # you must replace this value with the graph diameter
    return -1

def avg_path_length():
    """
    hardcode and return the average path length
    (run the function called “Avg. Path Length”) within Gephi
    :return:
    """
    # you must replace this value with the avg path length
    return -1
