FROM erikedin/slackrest:1.0rc2
LABEL maintainer="Erik Edin <erikedin.name@gmail.com>"

ENV PYTHONUNBUFFERED=0

COPY requirements.txt /app/requirements.txt

RUN pip install -r /app/requirements.txt

COPY . /app/

CMD ["python", "/app/niancatslack.py"]
