import aiml
import os

if os.getcwd() != "/home/pi/AIML":
    os.chdir("/home/pi/AIML")

kernel = aiml.Kernel()

if os.path.isfile("bot_brain.brn"):
    kernel.bootstrap(brainFile = "bot_brain.brn")
else:
    kernel.bootstrap(learnFiles = "std-startup.xml", commands = "load aiml b")
    #kernel.saveBrain("bot_brain.brn")

# session handling
# Get session info as dictionary. Contains the input
# and output history as well as any predicates known
sessionId = 12345
#kernel.setPredicate("cat", "Nina", sessionId)

# kernel now ready for use
while True:
    message = input("Enter your message >> ")
    if message == "quit":
        exit()
    elif message == "save":
        kernel.saveBrain("bot_brain.brn")
    elif message == "reload":
        kernel.respond("load aiml b")
    elif message == "session":
        sessionData = kernel.getSessionData(sessionId)
        print(sessionData)
    else:
        print(kernel.respond(message))
