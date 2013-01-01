#!/usr/bin/python
import sys
import re
import os
import shutil
import subprocess
import time

class Parser:

  def __init__(self):
    self.config = {}
    self.tests  = []
    self.benchmarks = []

  def parse(self,filename):
    configR = re.compile(r"config\s+(\{(.*?)\}){1,1}?(.*)", re.DOTALL)
    testsR  = re.compile(r"(.*?)test\s+(\w+)\s+(\{.*?\}){1,1}?(.*)", re.DOTALL)
    benchmarkR = re.compile(r"(.*?)benchmark\s+(\w+)\s+(\{.*?\}){1,1}?(.*)", re.DOTALL)

    try:
      file = open(filename,"r")
      src = file.read()
      old_src = None
  
      while  src != old_src:
        src.strip()
  
        #parse the configuration
        cur_expr = configR.match(src)
        if cur_expr:
          old_src = src

          #get the inner expression
          src = str(cur_expr.group(3))
          #extract the configuration
          self.__parse_config(str(cur_expr.group(1)))
          continue
  
        #parse the tests
        cur_expr = testsR.match(src)
        if cur_expr:
          test = self.__parse_expr(str(cur_expr.group(3)), str(cur_expr.group(2)))
          self.tests.append(test)
          old_src = src
          src = str(cur_expr.group(4))
          continue
  
        #parse the benchmarks
        cur_expr = benchmarkR.match(src)
        if cur_expr:
          old_src = src
          src = str(cur_expr.group(4))
          benchmark = self.__parse_expr(str(cur_expr.group(3)), str(cur_expr.group(2)))
          self.benchmarks.append(benchmark)
          continue
        
        old_src = src
  
    except Exception,e:
      print("""
          unable to parse grammar on input file. pleas check your syntax:
          faled on token: """ + str(e))
  
  def __parse_expr(self,body, test):
    exprR  = re.compile(r".*?\{(.*)\}", re.DOTALL)
   
    #this is the block containing all the tests
    cur_expr = exprR.match(body)
    locs = self.config.copy() # create a local copy of the global settings 
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
          pkgs.append(self.__package_testcase(exp.strip('\n').strip(), locs, test))
      return pkgs
    else:
      print('fail in parse_expr')
      raise Exception( 'invalid expression while parsing token: ' + body)

  def __package_testcase(self, exp, config, test):
    return {'name':test, 'exp':exp, 'config':config}
  
  def __parse_config(self, body):
    exprR  = re.compile(r".*?\{(.*)\}.*", re.DOTALL)
    cur_expr = exprR.match(body)
    if cur_expr:
      exprs = str(cur_expr.group(1))
      for expr in exprs.split(';'):
        if expr.find('=') != -1:
          key, val = expr.split('=')
          self.config[key.strip('\n;{} ').strip()] = val.strip('\n;').strip()
    else:
      print('fail in parse_globals')
      raise Exception('invalid expression while parsing token: ' + body)
