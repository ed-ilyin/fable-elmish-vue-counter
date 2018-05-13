import fable from 'rollup-plugin-fable'
const path = require('path')
var fs = require("fs");

function resolve(filePath) {
    return path.resolve(__dirname, filePath)
}
function forceGet(obj, path, errorMsg) {
    function forceGetInner(obj, head, tail) {
      if (head in obj) {
        var res = obj[head];
        return tail.length > 0 ? forceGetInner(res, tail[0], tail.slice(1)) : res;
      }
      throw new Error(errorMsg.replace("{0}", path));
    }
    var parts = path.split('.');
    return forceGetInner(obj, parts[0], parts.slice(1));
  }
  
const packageJson = JSON.parse(fs.readFileSync(resolve('../package.json')).toString());
var errorMsg = "{0} missing in package.json";
export default {
    input: resolve(path.join("..", forceGet(packageJson, "fable.entry", errorMsg))),
    output: {
        file: resolve('../build/app.js'),
        format: 'cjs'
    },
    plugins: [fable({})],
}
