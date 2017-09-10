Niancat is a Slack bot we use to aid us in solving a word puzzle, published by a Swedish newspaper every day.

The puzzle is nine shuffled letters, and the goal is to find the Swedish nine letter word that uses all letters. The bot
aids us in checking if a word is in the dictionary, and that it matches the puzzle.

The bot furthermore aids us in keeping track of who has solved the puzzle, and keeping a list of funny words which are
not correct solutions.

# Development
I use this bot to try out stuff I think is kind of interesting. This version of the bot was initially developed in
Scala, but I'm moving towards a micro-service architecture for the bot. Currently the bot consists of two services, only
one of which is micro.

The first larger service is a REST service, written in Scala, which contains all business logic for the bot. The Slack
client is in the second Python service, which only connects to Slack and relays all commands to the REST service.

The bot runs on a Kubernetes cluster, consisting of three Raspberry Pi 3.

# License
Apache License 2.0. See LICENSE.