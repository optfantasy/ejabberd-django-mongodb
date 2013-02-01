import sys

from fabric.api import run, local, cd, env, roles, execute
import requests

env.roledefs = {
    'deploy': ['emptydeploy'],
    'master': ['empty'],
    'slave': ['empty2']
    }
env.hosts = ['empty', 'empty2']
env.user = 'ejabberd'

HOME_FOLDER = '/home/ejabberd'


# Process
def process_presetup():
    with cd(HOME_FOLDER):
        run("touch presetup")

@roles('master')
def process_master_setup():
    with cd(HOME_FOLDER):
        run('touch master_setup')

@roles('slave')
def process_slave_setup():
    with cd(HOME_FOLDER):
        run('touch slave_setup')

def process_postsetup():
    with cd(HOME_FOLDER):
        run('touch postsetup')


# Command
def deploy():
    execute(process_presetup)
    execute(process_master_setup)
    execute(process_slave_setup)
    execute(process_postsetup)

