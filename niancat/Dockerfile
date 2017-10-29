FROM openjdk:8u131-jre
LABEL maintainer="Erik Edin <erikedin.name@gmail.com>"

ARG NIANCAT_JAR

RUN useradd -m user
USER user
WORKDIR /app

COPY ${NIANCAT_JAR} /app/niancat.jar

CMD ["java", "-jar", "/app/niancat.jar"]
