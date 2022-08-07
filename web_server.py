from flask import Flask, request, Response
from zugai import RunException, run, latex_png

app = Flask(__name__, static_folder='web-client/build/static')
formats = {
   'boxes':      'text/html',
   'english':    'text/plain',
   'logic':      'text/plain',
   'structure':  'text/plain',
   'xbar-latex': 'application/x-tex',
   'xbar-html':  'text/html',
   'xbar-json':  'application/json',
   'xbar-svg':   'image/svg+xml',
   'xbar-png':   'image/png',
}
usage = 'Usage:\n' + ''.join(f'GET /zugai?text=jadi&to={f}\n' for f in sorted(formats))

def cors(response):
    response.headers['Access-Control-Allow-Origin'] = '*'
    return response

@app.route('/zugai', methods=['GET'])
def zugai():
    args = request.args
    to = args.get('to')
    if to not in formats:
        return cors(Response(usage, status=400, mimetype='text/plain'))
    mimetype = formats[to]
    text = args.get('text', default='')
    try:
        out = None
        if to == 'xbar-png':
            with latex_png(text) as f: out = f.read()
        else:
            out = run("parsing", ['zugai-exe', f'--to-{to}'], input=text.encode())
        return cors(Response(out, mimetype=mimetype))
    except RunException as e:
        return cors(Response(str(e), status=500, mimetype='text/plain'))

