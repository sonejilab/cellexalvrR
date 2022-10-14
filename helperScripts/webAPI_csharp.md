# What do we need to implement in R to replace the database and all other files?

The c\# function inputReader.ReadFolderCoroutine() touches
  referenceManager.configManager.ReadConfigFiles
  database.InitDatabase
  mdsReader.ReadMDSFiles (could be already prepared here: ScarfManager.instance.LoadPreprocessedDataCouroutine)
  inputReader.StartServer
  inputReader.ReadFacsFiles
  inputReader.ReadNetworkFiles (networkReader.ReadNetworkFilesCoroutine)
  inputReader.ReadAnnotationFile
  inputReader.ReadSelectionFile

## how to interact with the web browser

Best seams to be [this](https://stackoverflow.com/questions/26857/how-do-you-programmatically-fill-in-a-form-and-post-a-web-page):

```
WebRequest req = WebRequest.Create("http://mysite/myform.aspx");
string postData = "item1=11111&item2=22222&Item3=33333";

byte[] send = Encoding.Default.GetBytes(postData);
req.Method = "POST";
req.ContentType = "application/x-www-form-urlencoded";
req.ContentLength = send.Length;

Stream sout = req.GetRequestStream();
sout.Write(send, 0, send.Length);
sout.Flush();
sout.Close();

WebResponse res = req.GetResponse();
StreamReader sr = new StreamReader(res.GetResponseStream());
string returnvalue = sr.ReadToEnd();
``` 

The [c# Json deserializtion explained](https://stackoverflow.com/questions/69730401/how-can-i-deserialize-a-json-into-a-structure).

I think that should do the trick.

