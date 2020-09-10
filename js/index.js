import CodeMirror from 'codemirror';
import 'codemirror/lib/codemirror.css';
import 'codemirror/addon/hint/show-hint';
import 'codemirror/addon/lint/lint';
import 'codemirror-graphql/hint';
import 'codemirror-graphql/lint';
import 'codemirror-graphql/mode';
import myGraphQLSchema from 'raw-loader!../tmp/schema.graphql';

window.CodeMirror = CodeMirror;
window.myGraphQLSchema = myGraphQLSchema;
