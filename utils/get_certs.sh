#!/bin/sh

SERVER=${1:-my.server.com}
PORT=${2:-993}
CERT_FOLDER=${3:-~/certs}

openssl s_client -connect ${SERVER}:${PORT} -showcerts 2>&1 < /dev/null | sed -ne '/-BEGIN CERTIFICATE-/,/-END CERTIFICATE-/p'| sed -ne '1,/-END CERTIFICATE-/p' > ${CERT_FOLDER}/${SERVER}.pem