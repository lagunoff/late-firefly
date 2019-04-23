const path = require('path');
const fs = require('fs');
const webpack = require('webpack');
// const tsConfig = require('./tsconfig.json');


module.exports = function() {
  return {
    module: {
      rules: [{
        test: /\.tsx?$/,
        loader: 'ts-loader',
//        options: { compilerOptions: tsConfig.compilerOptions, },
      }, {
        test: /\.css$/,
        loaders: ['style-loader', 'css-loader'],
      }]
    },
    resolve: {
      extensions: ['.js', '.jsx', '.json', '.ts', '.tsx'],
      alias: {
        'typescript-sdom': path.join(__dirname, './packages/typescript-sdom'),
      },
    },
    devtool: 'off',
    mode: 'development',
  };
};
