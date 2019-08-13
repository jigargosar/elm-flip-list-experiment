const path = require('path')
const HtmlWebpackPlugin = require('html-webpack-plugin')
const { CleanWebpackPlugin } = require('clean-webpack-plugin')

const NODE_ENV = process.env.NODE_ENV
const isProduction = NODE_ENV === 'production'
// console.log('NODE_ENV', NODE_ENV)

// const mode = isProduction ? 'production' : 'development'
// console.log('mode', mode)

// https://webpack.js.org/configuration/
module.exports = {
  // mode: mode,
  entry: './src/index.js',
  output: {
    publicPath: '/',
  },
  resolve: {
    extensions: ['.js', '.elm'],
  },
  module: {
    rules: [
      { test: /\.css$/, loader: ['style-loader', 'css-loader'] },
      {
        test: /\.elm$/,
        use: [
          {
            loader: 'elm-webpack-loader',
            options: { optimize: isProduction },
          },
        ],
      },
    ],
  },
  plugins: [
    new CleanWebpackPlugin(),
    new HtmlWebpackPlugin({ template: './src/index.html' }),
  ],
  // https://webpack.js.org/configuration/stats/
  // stats: 'errors-warnings',
  stats: {
    children: false,
    modules: false,
  },
  // devtool: isProduction ? 'source-map' : 'eval-source-map',
  // devtool: isProduction ? 'source-map' : false,
  devtool: false,
  // https://webpack.js.org/configuration/dev-server/
  devServer: {
    historyApiFallback: true,
    overlay: {
      warnings: true,
      errors: true,
    },
    hot: false,
    // hotOnly: true,
  },
}
