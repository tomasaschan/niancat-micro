import connexion

def search():
    print("GET /puzzle")
    return "ABCDEFGHI"


def post(puzzle):
    print("POST: /puzzle", puzzle)
