class IncomingMessage(object):
    def __init__(self, message, channel_id, user_id):
        self.message = message
        self.channel_id = channel_id
        self.user_id = user_id
