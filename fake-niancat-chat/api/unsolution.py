def get(user):
    print("GET /unsolution/{}".format(user))
    return [
        {"user": "foo", "unsolutions": ["ABCDEFGHI", "DEFGHI"]},
        {"user": "bar", "unsolutions": ["GHI"]}
    ]


def post(user, unsolution):
    print("POST /unsolution/{}:".format(user), unsolution)
