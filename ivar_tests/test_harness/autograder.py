#!/usr/bin/python
import sys
import re
import os
import shutil
import subprocess
import time

def processExists(proc):
   ps = subprocess.Popen("ps -A", shell=True, stdout=subprocess.PIPE)
   ps_pid = ps.pid
   output = ps.stdout.read()
   ps.stdout.close()
   ps.wait()

   for line in output.split("\n"):
      if line != "" and line != None:
        fields = line.split()
        pid = fields[0]
        pname = fields[3]
        if(pname == proc):
         return True

def findInSubdirectory(filename, subdirectory=''):
    if subdirectory:
        path = subdirectory
    else:
        path = os.getcwd()
    for root, dirs, names in os.walk(path):
        if filename in names:
            return os.path.join(root, filename)
    #raise Exception('File not found')
    return None;

def do_compile(path, makefile, arg=''):
  print("\n\ncompiling with makefile: " + path+'/'+makefile)
  shutil.copy2(os.getcwd()+"/"+makefile, path)
  curmkfile = makefile.split('/')[-1]
  success =  os.system("make -C " + path + " -f " + curmkfile + " " + arg)
  os.remove(path+"/"+curmkfile)
  print("compile complete\n\n")
  if success != 0:
    print("compile failed!")
    failed.write(path+"\n")
    #raise Exception('Could not compile')


def remove_main(path):
  main = re.compile(r"(.*?)(\s*main.*?\{.*?\})(.*)", re.DOTALL)
  mainN = re.compile(r"(.*?)(\s*main.*?\{.*?\})(.*)", re.DOTALL)
  mainR = re.compile(r"(.*?)(int\s*main.*?\{.*?\})(.*)", re.DOTALL)
  mainS = re.compile(r"(.*?)(void\s*main.*?\{.*?\})(.*)", re.DOTALL)
  mainRN = re.compile(r"(.*?)(int\s*main.*?\n.*?\{.*?\})(.*)", re.DOTALL)
  mainSN = re.compile(r"(.*?)(void\s*main.*?\n.*?\{.*?\})(.*)", re.DOTALL)
  try:
    file = open(path,"r")
    code = file.read()
    file.close()
    match = mainR.match(code)
    if match is None:
     print("no int return found, trying with void")
     match = mainS.match(code)

    if match is None:
     print("no int or void return found, trying with int and a newline")
     match = mainSN.match(code)

    if match is None:
     print("no int or void return found, trying with void and a newline")
     match = mainRN.match(code)

    if match is None:
     print("no int or void return found, trying with nil and a newline")
     match = mainN.match(code)

    if match is None:
     print("no int or void return found, trying with nil")
     match = main.match(code)

    if match:
      shutil.move(path, path+'.testing_backup')
      newfile = open(path, "w+")
      newcode = str(match.group(1)) + str(match.group(3))
      newfile.write(newcode)
      newfile.close()
      return True
    else:
      print("no main found. trying to compile")
      return False

  except Exception, e:
    print(e)
    print("could not remove main!")


def restore_main(path):
  try:
      shutil.move(path+'.testing_backup',path)
      print("main method restored after testing")
  except:
    print("could not restore main!")

def run_test(expected, output, path, testcase, input='', asargs=False):
  executable = testcase['executable']
  timeout    = testcase['settings']['timeout']
  input = testcase['settings']['testdir']+'/'+input
  try:
      outpath = path+"/"+output
      print ("outpath: " + outpath)
      tmp = None
      os.system("touch " + outpath)
      print("created outfile: " + str(os.path.exists(outpath)))

      if input:
        #add support for arguments to be passed in
        if asargs:
          args = open(os.getcwd()+'/'+input,'r').read()
          print("args: " + args)

          #os.system(path+"/"+executable + " " +args + " > " + outpath)

          #TODO: unfortunately, python does not play nice with programs with args
          # and file redirection. This is very unfortunate as it destroys our guard
          # against infinite loops. This needs to get fixed. not sure why the above doesn't work.
          # below works, but uses a subprocess which must return to get the result

          #BEGIN HACK
          p = subprocess.Popen([path+'/'+executable, args], stdout=subprocess.PIPE)
          res = p.communicate()
          if res: 
            f = open(outpath,'w+')
            f.write(res[0])
            f.close()
          #END HACK

        else:
          #add support for arguments to be passed through the stdin
          os.system(path+"/"+executable+" < "+input+" > "+outpath)
      else:
        #add support for the program to be run with no input
        os.system(path+"/"+executable+" > "+outpath)

      runtime = 0
      #wait for the program to finish
      while processExists(path+"/"+testcase['executable']+"_test"):
         time.sleep(1)
         runtime +=1
         if runtime > timeout:
           break

      tmp = open(outpath,"r+")
      got = tmp.read()
      tmp.close()
      print(
      """
      =======================================================
      =                                                     =
              GOT:  {0}
      =                                                     =
         EXPECTED:  {1}
      =                                                     =                                        
      =======================================================
      """).format(got.strip(), expected) 
      os.remove(outpath) #delete the file so we have a fresh one next test
      results = open(path+"/test_results.results", "a+")
     
      if got.strip('\n').strip() == expected.strip('\n').strip():
        results.write("PASSED: " + got.strip() +" || "+expected+"\n")
      else:
        results.write("FAILED: " + got.strip() +" || "+expected+"\n")

      checked.write("TEST_CASE: " + path+"\n")
  except Exception, e:
      print("ERROR: exception while running tests: " + str(e))

def run_with_input(testcase, student):
  try:

     #search for the student's file
     path = findInSubdirectory(testcase['executable']+".c", testcase['settings']['assndir']+'/'+student)

     print(path)
     if path is None:
       print("could not find file: " + testcase['executable']+".c under root: " + testcase['settings']['assndir']+'/'+student)
       raise(Exception)
     #strip off the file to get the path 
     pattern = re.compile(r"(.*)/.*$")
     match = pattern.match(path)
     path = match.groups()[0]

     #parse the testcase
     #input,output,makefile,expected = testcase['exp'].strip(';').split(":")
     exprs = testcase['exp'].strip(';').split(":")

     num_args = len(exprs)

     #if args was specified, then run with the input as args
     if num_args == 5:
       asargs = exprs[0]
       input = exprs[1]
       output = exprs[2]
       makefile = exprs[3]
       expected = exprs[4]
       do_compile(path,testcase['settings']['testdir']+'/'+makefile)
       run_test(expected, output, path, testcase, input, True)
     #if just input was given, treat it as input to the stdin
     elif num_args == 4: 
       input = exprs[0]
       output = exprs[1]
       makefile = exprs[2]
       expected = exprs[3]
       do_compile(path,testcase['settings']['testdir']+'/'+makefile)
       run_test(expected, output, path, testcase, input)
     #if no input file was given, run the program unconditionally and record the output in the output file
     elif num_args == 3:
       output = exprs[0]
       makefile = exprs[1]
       expected = exprs[2]
       do_compile(path,testcase['settings']['testdir']+'/'+makefile)
       run_test(expected, output, path, testcase)



     #we can run simple tests with input directed in or not
     #if input == "":
     # run_test(expected, output, path, testcase)
     #else:
     # run_test(expected, output, path, testcase, input)

  except Exception,e:
    print e
    print "error in running with input"

def run_test_suite(testcase, student):
  try:

     path = findInSubdirectory(testcase['executable']+".c", testcase['settings']['assndir']+'/'+student)
     pattern = re.compile(r"(.*)/.*$")
     match = pattern.match(path)
     path = match.groups()[0]

     for file in (testcase['exp']).split(","):
       cpystr = os.getcwd()+"/"+testcase['settings']['testdir']+'/'+file
       print("copying: " + cpystr + " to " + path)
       shutil.copy2(cpystr, path)

     #if the student wrote a main function, back the file up and link against our main for testing
     hasmain = remove_main(path+'/'+(testcase['executable'])+".c")

     #everything setup, now compile and run 
     do_compile(path, testcase['settings']['testdir']+"/Makefile."+testcase['executable']+"_test")

     #restore the student's code
     if hasmain:
      restore_main(path+'/'+(testcase['executable'])+".c")
#CSZ: added in testcase['executable'] line for multiple test suite runners
     os.system(path+"/"+testcase['executable']+"_test" + " " + path +"/test_suite_results"+testcase['executable']+".results")

     #cleanup the directory
     for file in (testcase['exp']).split(","):
       if file.find("Makefile") > -1:
         continue
       toremove = path+'/'+file
       print("removing: " + toremove)
       os.remove(toremove)

     timeout = int(testcase['settings']['timeout'])
     runtime = 0
     #wait for the program to finish
     while processExists(path+"/"+testcase['executable']+"_test"):
        time.sleep(1)
        runtime +=1
        if runtime > timeout:
          break

     checked.write("TEST SUITE: " + path+"\n")
  except Exception,e:
    print("error in run_testsuite " + str(e))


def parse_tests(filename):
  setupR = re.compile(r"setup\s+(\{(.*?)\}){1,1}?(.*)", re.DOTALL)
  testsR = re.compile(r"(.*)tests\s+(\w+)\s+(\{.*?\}){1,1}?(.*)", re.DOTALL)
  suiteR = re.compile(r"(.*)suite\s+(\w+)\s+(\{.*?\}){1,1}?(.*)", re.DOTALL)
  tests = []
  globes = {}

  try:
    file = open(filename,"r")
    src = file.read()
    old_src = None

    while  src != old_src:
      src.strip()

      cur_expr = setupR.match(src)
      if cur_expr:
        old_src = src
        src = str(cur_expr.group(3))
        globes = parse_globals(str(cur_expr.group(1)))
        continue

      cur_expr = testsR.match(src)
      if cur_expr:
        test = parse_expr(str(cur_expr.group(3)), globes, str(cur_expr.group(2)))
        for d in test:
           d['type'] = 'test'
        tests.append(test)
        old_src = src
        src = str(cur_expr.group(4))
        continue

      cur_expr = suiteR.match(src)
      if cur_expr:
        old_src = src
        src = str(cur_expr.group(4))
        suite = parse_expr(str(cur_expr.group(3)), globes, str(cur_expr.group(2)))
        for d in suite:
           d['type'] = 'suite'
        tests.append(suite)
        continue

      
      old_src = src
      return tests

  except Exception,e:
    print("""
        unable to parse grammar on input file. pleas check your syntax:
        faled on token: """ + str(e))


  
def parse_expr(body, globes, test):
  exprR  = re.compile(r".*?\{(.*)\}", re.DOTALL)
 
  #this is the block containing all the tests
  cur_expr = exprR.match(body)
  locs = globes.copy() # create a local copy of the global settings 
                       # so local settings take effect here only
  if cur_expr:
    exprs = str(cur_expr.group(1))
    pkgs = []
    #semicolon delimited commands. 
    for exp in exprs.split(";"):
      exp = exp.strip('\n').strip()
      if exp == "" or exp.find("#") == 0:
        continue

      if exp.find('=') != -1:
        key,val = exp.split('=')
        locs[key.strip('\n').strip()] = val.strip(';').strip('\n').strip()
        continue
      else:
        pkgs.append(package_testcase(exp.strip('\n').strip(), locs, test))
    return pkgs
  else:
    print('fail in parse_expr')
    raise Exception( 'invalid expression while parsing token: ' + body)

def parse_globals(body):
  exprR  = re.compile(r".*?\{(.*)\}.*", re.DOTALL)
  cur_expr = exprR.match(body)
  dict = {}
  if cur_expr:
    exprs = str(cur_expr.group(1))
    for expr in exprs.split(';'):
      if expr.find('=') != -1:
        key, val = expr.split('=')
        dict[key.strip('\n;{} ').strip()] = val.strip('\n;').strip()
    return dict
  else:
    print('fail in parse_globals')
    raise Exception('invalid expression while parsing token: ' + body)

    
def package_testcase(exp, settings, test):
  return {'executable':test, 'exp':exp, 'settings':settings}


def test_runner(filename):
  tests = parse_tests(filename)
  assignmentError = ""
  studentError = ""
  for test in tests:
    config = tests[0][0]['settings']
    try:
       dir = os.listdir(config['assndir'])

       for student in dir:
         studentError = student
         repo = os.listdir(config['assndir']+'/'+student)
         if repo == []:
           unchecked.write(student)
         else:
           for testcase in test:
             assignmentError = testcase['executable']

             if testcase['type'] == 'test':
               run_with_input(testcase, student)
             else:
               run_test_suite(testcase, student)
    except Exception,e:
      print("fatal exception in test runner: " + str(e))
      failed.write(studentError +"/"+assignmentError+" "+str(e)+"\n")

def cleanup(filename):
   tests = parse_tests(filename)
   for test in tests:
     config = tests[0][0]['settings']
     dir = os.listdir(config['assndir'])
     for student in dir:
       repo = os.listdir(config['assndir']+'/'+student)
       if repo == []:
         continue;
       else:
         for testcase in test:
           if testcase['type'] == 'test':
             #search for the student's file
             path = findInSubdirectory(testcase['executable']+".c", testcase['settings']['assndir']+'/'+student)

             if path is None:
               continue

             #strip off the file to get the path 
             pattern = re.compile(r"(.*)/.*$")
             match = pattern.match(path)
             path = match.groups()[0]
             print(path)

             #all set. compile!
             do_compile(path,testcase['settings']['testdir']+'/'+testcase['settings']['cleanup'], 'clean')

checked    = open("checked.txt", "w")
unchecked  = open("unchecked.txt", "w")
failed     = open("failed.txt", "w")

if len(sys.argv) > 1:
  print(sys.argv[1])
  if sys.argv[1] == '--clean':
   cleanup(sys.argv[2])
  else:
   test_runner(sys.argv[1])
else:
  print("error: usage is either --clean <config file> or just <config file>")

#close our files to be nice
checked.close() 
unchecked.close()
failed.close()
      
      

