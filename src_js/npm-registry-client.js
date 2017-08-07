const {promisify} = require('util');
const NpmRegistryClientBase = require('npm-registry-client');

class NpmRegistryClient extends NpmRegistryClientBase {

  constructor(config) {
    super(config);
    this.get = promisify(this.get.bind(this));
  }
}
module.exports = NpmRegistryClient;
