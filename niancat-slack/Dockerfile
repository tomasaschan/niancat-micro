FROM arm32v7/debian:jessie
LABEL maintainer="Erik Edin <erikedin.name@gmail.com>"

ENV PYTHONUNBUFFERED=0

COPY requirements.txt /app/requirements.txt

RUN apt update && apt install -y python3 python3-pip
RUN pip3 install -r /app/requirements.txt

COPY . /app/

CMD ["python3", "/app/niancatbot/niancatbot.py"]