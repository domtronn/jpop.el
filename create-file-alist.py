#!/usr/bin/python
import os, re, sys, json, getopt

optlist, args = getopt.getopt(sys.argv[1:], "i:")
filter_regexp = args[1].split(",") if args[1] else ""
project_file  = args[0]

invert = False;
for o, a in optlist:
    if o == "-i": invert = a.split(",")

def invert_selection( resultant_files, invert_regexp ):
    result = []
    for r in invert_regexp:
        regex = re.compile(r)
        result = result + [f for f in resultant_files if regex.search(f)]
    return result

def normalise_invert( resultant_files, invert_regexp ):
    modified_files = {}
    for file_id, files in resultant_files.iteritems():
        for r in invert_regexp:
            regex = re.compile(r)
            if regex.search(file_id):
                modified_files[regex.sub('\\1',file_id)] = resultant_files[file_id]
    return modified_files
                
def create_from_path( path, filter_regexp, invert_regexp ):
    resultant_files = []
    for root, dirs, files in os.walk(os.path.expanduser(path)):
        for name in files:
            resultant_files.append(os.path.join(root, name))

    for r in filter_regexp:
        regex = re.compile(r)
        resultant_files = [f for f in resultant_files if not regex.search(f)]

    if invert_regexp:
        resultant_files = invert_selection(resultant_files, invert_regexp) 

    result_dict = {}
    for f in resultant_files:
        if os.path.basename(f) in result_dict:
            result_dict[os.path.basename(f)].append(os.path.dirname(f) + "/" + os.path.basename(f))
        else:
            result_dict[os.path.basename(f)] = [os.path.dirname(f) + "/" + os.path.basename(f)]
    
    if invert_regexp:
        result_dict = normalise_invert(result_dict, invert_regexp)

    result = {}
    result[os.path.basename(project_file)] = result_dict
    
    print json.dumps(result)
        
def create_from_json( project_file, filter_regexp, invert_regexp ):
    project_json = json.loads(open(project_file).read())

    project_dict = { project_json['id']: project_json['dirs'] }
    if 'libs' in project_json:
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

    if invert_regexp:
        for project_id, resultant_files in resultant_dict.iteritems():
            resultant_dict[project_id] = invert_selection(resultant_files, invert_regexp) 

    for project_id, resultant_files in resultant_dict.iteritems():
        result_dict = {}
        for f in resultant_files:
            if os.path.basename(f) in result_dict:
                if os.path.dirname(f) + "/" + os.path.basename(f) in result_dict[os.path.basename(f)]:
                    continue
                result_dict[os.path.basename(f)].append(os.path.dirname(f) + "/" + os.path.basename(f))
            else:
                result_dict[os.path.basename(f)] = [os.path.dirname(f) + "/" + os.path.basename(f)]
        resultant_dict[project_id] = result_dict
        
    if invert_regexp:
        for project_id, resultant_files in resultant_dict.iteritems():
            resultant_dict[project_id] = normalise_invert(resultant_files, invert_regexp)

    print json.dumps(resultant_dict)


if os.path.isfile(project_file):
    create_from_json( project_file, filter_regexp, invert )
else:
    create_from_path( project_file, filter_regexp, invert )
