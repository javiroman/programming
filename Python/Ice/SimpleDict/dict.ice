module fase {
  module bindings {
    module logging {

      dictionary<string, string> Message;

      interface LoggingServer {
        Message NewMessage(string msg);
        void log(Message msg);
      };

    };
  };
};