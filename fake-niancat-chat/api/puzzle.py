import connexion

def search():
    print("GET /puzzle")
    return {"response_type": "reply", "message": "ABCDEFGHI"}


def post(puzzle):
    print("POST: /puzzle", puzzle)
    return {"response_type": "reply", "message": "Nian sattes till {}".format(puzzle)}
