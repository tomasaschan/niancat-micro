class RouteContext(object):
    def __init__(self, outbound_message_queue, incoming_message):
        self.outbound_message_queue = outbound_message_queue
        self.incoming_message = incoming_message

    def route(self, response):
        response_channel = self.incoming_message.user_id
        self.outbound_message_queue.enqueue(response['message'], response_channel)