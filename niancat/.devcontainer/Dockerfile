FROM mozilla/sbt

RUN useradd -m dev -s /bin/bash

RUN mkdir -p /workspaces/niancat-micro && chown -R dev:dev /workspaces/niancat-micro
RUN mkdir -p /home/dev/.vscode-server-insiders && chown -R dev:dev /home/dev/.vscode-server-insiders
RUN mkdir -p /home/dev/.sbt && chown -R dev:dev /home/dev/.sbt

USER dev

RUN echo "\n\nif [ -d .bashrc.d ]; then\n  for rc in .bashrc.d/*rc; do\n    . \$rc\n  done\nfi\n" >> /home/dev/.bashrc

