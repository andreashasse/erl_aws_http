# erl_aws_http
Helps you to build libraries that consumes typical amazon http apis.

## Getting started

### Amazon credentials
You should add your amazon credentials in the app config of erl_aws_http,
Eg: [aws_http.app.src](https://github.com/anha0825/erl_aws_http/blob/master/src/aws_http.app.src). 
If no profile is sent in the opts argument to a http call the profile called default will be used.

Check out [erl_aws_lambda](https://github.com/anha0825/erl_aws_lambda) if you want to see how this library is used.
