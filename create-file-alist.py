#!/usr/bin/python
import os, re, sys, json

filter_regexp = sys.argv[2].split(",") if sys.argv[2] else ""
project_file = sys.argv[1]

def create_from_path( path, filter_regexp ):
    resultant_files = []
    for root, dirs, files in os.walk(os.path.expanduser(path)):
        for name in files:
            resultant_files.append(os.path.join(root, name))

    for r in filter_regexp:
        regex = re.compile(r)
        resultant_files = [f for f in resultant_files if not regex.search(f)]
        
    result_dict = {}
    for f in resultant_files:
        if os.path.basename(f) in result_dict:
                # if any(os.path.dirname(f) + "/" in f for f in result_dict[os.path.basename(f)]):
                #     continue
            result_dict[os.path.basename(f)].append(os.path.dirname(f) + "/")
        else:
            result_dict[os.path.basename(f)] = [os.path.dirname(f) + "/"]
    result = {}
    result[os.path.basename(project_file)] = result_dict
    
    print json.dumps(result_dict)
        
def create_from_json( project_file, filter_regexp ):
    project_json = json.loads(open(project_file).read())

    project_dict = { project_json['projectId']: project_json['project'] }
    for lib in project_json['libs']:
        project_dict[ lib['id'] ] = [ lib ]

    resultant_dict = {}
    for project_id, project in project_dict.iteritems():
        resultant_dict[project_id] = []
        for project_dir in project:
            for root, dirs, files in os.walk(os.path.expanduser(project_dir['dir'])):
                for name in files:
                    resultant_dict[project_id].append(os.path.join(root, name))

    for r in filter_regexp:
        regex = re.compile(r)
        for project_id, resultant_files in resultant_dict.iteritems():
            resultant_dict[project_id] = [f for f in resultant_files if not regex.search(f)]

    for project_id, resultant_files in resultant_dict.iteritems():
        result_dict = {}
        for f in resultant_files:
            if os.path.basename(f) in result_dict:
                # if any(os.path.dirname(f) + "/" in f for f in result_dict[os.path.basename(f)]):
                #     continue
                result_dict[os.path.basename(f)].append(os.path.dirname(f) + "/")
            else:
                result_dict[os.path.basename(f)] = [os.path.dirname(f) + "/"]
        resultant_dict[project_id] = result_dict

    print json.dumps(resultant_dict)


if os.path.isfile(project_file):
    create_from_json( project_file, filter_regexp )
else:
    create_from_path( project_file, filter_regexp )
