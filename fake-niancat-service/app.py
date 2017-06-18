#!/usr/bin/env python3
import connexion
from connexion.resolver import RestyResolver

app = connexion.App(__name__, specification_dir='../apis')
app.add_api('niancat.yaml', resolver=RestyResolver('api'))
app.run(port=8081)
