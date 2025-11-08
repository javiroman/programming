"""
Multipart upload allows you to upload a single object as a
set of parts. Each part is a contiguous portion of the object's data.
You can upload these object parts independently and in any order.
If transmission of any part fails, you can retransmit that part
without affecting other parts. After all parts of your object are
uploaded, Amazon S3 assembles these parts and creates the object.
In general, when your object size reaches 100 MB, you should consider
using multipart uploads instead of uploading the object in a single operation.
"""
class CopyFile:
    'Documentation here'
    def __init__(selfs):
        pass

    def copyFile(self):
        print("Hello copyFile!")

class CopyFolder:
    'Documentation here'
    def __init__(selfs):
        pass

    def copyFolder(self):
        print("Hello copyFolder!")