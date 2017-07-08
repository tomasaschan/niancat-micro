import time

def before_feature(self, feature):
    # Sleep to allow the niancat service to start up
    time.sleep(5)
