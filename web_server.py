from flask import Flask, request, Response
import subprocess

app = Flask(__name__)
formats = ("english", "logic", "xbar-latex", "xbar-html", "xbar-json", "xbar-svg")
usage = "Usage:\n" + "".join(f"GET /zugai?text=jadi&to={f}\n" for f in formats)

@app.route('/zugai', methods=['GET'])
def zugai():
    args = request.args
    to = args.get('to')
    if to not in formats:
        return Response(usage, status=400, mimetype="text/plain")
    text = args.get('text', default='')
    run = subprocess.run(["zugai-exe", f"--to-{to}"], input=text.encode(), capture_output=True)
    if run.returncode != 0:
        return Response(run.stderr, status=500, mimetype="text/plain")
    return Response(run.stdout, mimetype="text/json" if "json" in to else "text/plain")
