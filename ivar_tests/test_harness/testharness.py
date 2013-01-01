#!/usr/bin/python

import parser
import sys

def main():
  output = parser.Parser()
  output.parse(sys.argv[1])
  print str(output.config) + "\n"
  print str(output.tests)  + "\n"
  print str(output.benchmarks) + "\n"

main()
  
