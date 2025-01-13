'''
Script Name: Prevalence Estimator Data Export Output Processing
Author: Nicholas Hollingshead, Cornell University
Description: Converts output files from the Prevalence Estimator Data Export
             Model to file formats appropriate for the Weighted Surveillance
             CWD Detection model for use in the Estimation Tool. The external
             model is available here: https://public.spdgt.com/app/wtsurv.
Inputs: 
  SpeedGoatOutputMatrix.csv
Outputs: 
  PrevalenceEstimatorData.xlsx
'''

import pathlib
import csv
import json
import os
import sys
import logging
import pandas as pd

model_metadata_log_file = '/data/attachments/info.html'
logging_path = '/data/attachments/execution_log.log'
attachments_json_path = pathlib.Path("/", "data", "attachments.json")

################### 
# Functions

def model_log_html(line='', html_element="p", filename=model_metadata_log_file):
    """
    Writes a single line to the model_metadata_log text file with specified HTML element.

    Args:
        line: The line to be written.
        filename: The name of the file.
        html_element: The HTML element tag to use (e.g., "h1", "h2", "p", "div").
    """
    with open(filename, 'a') as f:
        f.write(f"<{html_element}>{line}</{html_element}>" + '\n') 

def add_item_to_json_file_list(file_path, new_item):
  """
  Adds a new item to the list within a JSON file.

  Args:
    file_path: Path to the JSON file.
    new_item: The item to be added to the list.

  Raises:
    FileNotFoundError: If the specified file does not exist.
    json.JSONDecodeError: If the file content is not valid JSON.
  """

  try:
    with open(file_path, 'r') as f:
      data = json.load(f)

    if isinstance(data, list):
      data.append(new_item)
    else:
      raise ValueError("The JSON file does not contain a list.")

    with open(file_path, 'w') as f:
      json.dump(data, f, indent=2) 

  except FileNotFoundError:
    print(f"Error: File '{file_path}' not found.")
    raise
  except json.JSONDecodeError:
    print(f"Error: Invalid JSON in '{file_path}'.")
    raise
  except ValueError as e:
    print(f"Error: {e}")
    raise
  
################
# LOGGING CONFIG

logging.basicConfig(level = logging.DEBUG, # Alternatively, could use DEBUG, INFO, WARNING, ERROR, CRITICAL
                    filename = logging_path, 
                    filemode = 'a', # a is append, w is overwrite
                    datefmt = '%Y-%m-%d %H:%M:%S',
                    format = '%(asctime)s - %(levelname)s - %(message)s')

# Uncaught exception handler
def handle_uncaught_exception(type, value, traceback):
  logging.error(f"{type} error has occurred with value: {value}. Traceback: {traceback}")
sys.excepthook = handle_uncaught_exception

#######
# MAIN

model_log_html("Model Exports", "h3")

# Read the CSV file into a Pandas DataFrame
try:
  model_output_csv = pathlib.Path("/data/SpeedGoatOutputMatrix.csv")
  df = pd.read_csv(model_output_csv) 
except Exception as e:
  model_log_html("ERROR", "h4")
  model_log_html("SpeedGoatOutputMatrix.csv not found or could not be imported by Pandas.")
  logging.error("SpeedGoatOutputMatrix.csv not found or could not be imported by Pandas.")
  sys.exit(1)

# Write the DataFrame to an Excel file
df.to_excel(pathlib.Path("/data/attachments/PrevalenceEstimatorData.xlsx"), sheet_name='Data', index=False)

# with pd.ExcelWriter('output.xlsx') as writer:
#     df.to_excel(writer, sheet_name='Sheet1')
#     writer.save() 
# os.getcwd()

# Generate the Output Matrix Attachment
attachment = {"filename": "PrevalenceEstimatorData.xlsx", "content_type": "application/vnd.openxmlformats-officedocument.spreadsheetml.sheet", "role": "downloadable"}
add_item_to_json_file_list(attachments_json_path, attachment)
  
model_log_html("Model exports successfully created.")
logging.info("Model output processing successfully completed.")