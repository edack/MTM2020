#Import the Z Open Automation Utilities libraries we need
from zoautil_py import MVSCmd, Datasets
from zoautil_py.types import DDStatement
import os

# Grab the environment variable for USER, which should be equal to your Zxxxxx userid
USERID = os.getenv('USER') 
dataset_to_list = "WORK"

target_dataset = USERID + "." + dataset_to_list
ds_members = Datasets.list_members(target_dataset)
print(ds_members)