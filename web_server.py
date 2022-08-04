from flask import Flask, request, Response
import subprocess

app = Flask(__name__)
formats = {
   'boxes':      'text/html',
   'english':    'text/plain',
   'logic':      'text/plain',
   'structure':  'text/plain',
   'xbar-latex': 'application/x-tex',
   'xbar-html':  'text/html',
   'xbar-json':  'application/json',
   'xbar-svg':   'image/svg+xml',
}
usage = "Usage:\n" + "".join(f"GET /zugai?text=jadi&to={f}\n" for f in sorted(formats))

def cors(response):
    response.headers["Access-Control-Allow-Origin"] = "*"
    return response

@app.route('/zugai', methods=['GET'])
def zugai():
    args = request.args
    to = args.get('to')
    if to not in formats:
        return cors(Response(usage, status=400, mimetype="text/plain"))
    text = args.get('text', default='')
    run = subprocess.run(["zugai-exe", f"--to-{to}"], input=text.encode(), capture_output=True)
    if run.returncode != 0:
        return cors(Response(run.stderr, status=500, mimetype="text/plain"))
    return cors(Response(run.stdout, mimetype=formats[to]))
