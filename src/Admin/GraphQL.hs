{-# OPTIONS_GHC -Wno-orphans #-}
module Admin.GraphQL where

import Language.Javascript.JMacro

import "this" Router
import "this" Widget

data GraphQlD = GraphQlD
  deriving stock (Show, Eq, Generic)

instance IsPage "GraphQl" GraphQlD where
  pageTemplate = id
  pageWidget GraphQlR{} GraphQlD{} = do
    let
      Theme{..} = theme
      js_ x = script_ [src_ x] ("":: Text)
    js_ "/dist/bundle.js"
    div_ [class_ "graphql"] do
      div_ [class_ "toolbar"] do
        button_ [id_ "send"] "Send"
      div_ [class_ "content", id_ "content"] ""
    toHtml [jmacro|
      var contEl = document.getElementById('content');
      var sendEl = document.getElementById('send');
      var value = localStorage.getItem('codeMirror1_value');
      var codeMirror1 = CodeMirror \x {contEl.appendChild(x)} {
        mode:'graphql',
        lineNumbers: true,
        value: value || ''
        // lint: {schema: myGraphQLSchema},
        // hintOptions: { schema: myGraphQLSchema}
      };
      var codeMirror2 = CodeMirror \x {contEl.appendChild(x)} {
        mode:'json',
        lineNumbers: true, value: ''
      };
      sendEl.addEventListener('click', \e {
        var body = { operationName: null, query: codeMirror1.getValue(), variables: {} };
        var headers = {'Content-Type': 'application/json'};
        fetch('https://graphql.imdb.com/index.html', {
          method: 'post',
          body: JSON.stringify(body),
          headers: headers
        }).then(\r -> r.json()).then(\resp {
          codeMirror2.setValue(JSON.stringify(resp, null, 2))
        })
      });
      window.addEventListener('beforeunload', \{
        localStorage.setItem('codeMirror1_value', codeMirror1.getValue());
      });
    |]
    [style|
      .graphql
        height: 100%
        display: flex
        flex-direction: column
      .content
        display: flex
        flex-direction: row
        height: 100%
        & > *
          width: 50%
          height: 100%
          box-sizing: border-box
          border: solid 1px rgba(0,0,0,0.1)
        & > * + *
          margin-left: #{unit}
      .toolbar
        justify-content: flex-end
        padding-bottom: #{unit}
        display: flex
        flex-direction: row
        justify-content: flex-end
        & > button
          height: #{unit * 4}
          padding: 0 #{unit * 2}
          text-transform: uppercase
      body
        margin: 0
        padding: #{unit}
        height: 100%
        position: absolute
        width: 100%
        box-sizing: border-box
    |]

  pageInit GraphQlR{} = pure GraphQlD
