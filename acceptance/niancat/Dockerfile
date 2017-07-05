FROM alpine

RUN apk update && apk add python3

COPY requirements.txt /app/requirements.txt
RUN pip3 install -r /app/requirements.txt

COPY . /app

WORKDIR /app

RUN adduser -D user
USER user