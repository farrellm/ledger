import os

j = os.environ.get("JUPYTER_PATH")
if j is not None:
    for p in j.split(":"):
        print(p)
