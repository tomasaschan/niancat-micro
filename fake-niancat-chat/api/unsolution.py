def get(user):
    print("GET /unsolution/{}".format(user))
    return {"response_type": "reply", "message": "Tack för att du är så fin. Så säger vi om **olösningar**."}


def post(user, unsolution):
    print("POST /unsolution/{}:".format(user), unsolution)
    return {"response_type": "reply", "message": "Satte olösning {}".format(unsolution)}
