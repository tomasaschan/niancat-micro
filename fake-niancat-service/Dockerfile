FROM arm32v7/debian
LABEL maintainer="Erik Edin <erikedin.name@gmail.com>"

RUN apt update && apt install -y python3 python3-pip

COPY requirements.txt /app/requirements.txt
RUN pip3 install -r /app/requirements.txt

COPY . /app/

CMD ["/usr/bin/python3", "/app/app.py"]
