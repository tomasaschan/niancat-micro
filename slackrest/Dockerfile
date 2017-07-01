FROM alpine
LABEL maintainer='Erik Edin <erikedin.name@gmail.com>'

RUN apk update && apk add python3 openssl

WORKDIR /var/cert
RUN openssl req -x509 -newkey rsa:2048 -keyout key.pem -out cert.pem -days 365 -subj '/CN=slack.com' -nodes -sha256

COPY requirements.txt /app/requirements.txt
RUN pip3 install -r /app/requirements.txt
COPY . /app/
WORKDIR /app