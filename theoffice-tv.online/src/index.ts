import { SDOM, h, create, Patch, RawPatch, preparePatch, actuate, Jet, applyPatch, discriminate } from 'typescript-sdom';
import db, { Db } from './db';
import jss from 'jss';
import preset from 'jss-preset-default';
jss.setup(preset());

import { parse, print, Route } from './routes';
import * as Home from './home';
import * as Season from './season';
import * as Episode from './episode';


// Map route tag to component
const routeToComponent: Record<Route['tag'], any> = {
  Home,
  Episode,
  Season,
};


// Model
export type Model = {
  page: {
    route: Route;
    model: any;
  };
};

// Action
export type Action =
  | { tag: 'RouteChange', value: Route }
  | { tag: '@Children', update: any, action: any }
;

// Update
export function update(action: Action, model: Model): RawPatch<Model> {
  switch (action.tag) {
    case 'RouteChange': {
      const route = action.value;
      const pageModel = routeToComponent[route.tag].init(db, route);
      return { $patch: { page: { route, model: pageModel } } };
    }
    case '@Children': {
      const patch = action.update(action.action, model.page.model);
      return { $at: ['page', 'model'], patch };
    }
  }
}

// View
export function view(db: Db) {
  const header = h.header(
    h('nav').attrs({ class: "navbar navbar-expand-lg navbar-light bg-light" }).childs(
      h.button({ class: "navbar-toggler", type: 'button', 'data-toggle': 'collapse', 'data-target': "#navbarTogglerDemo01", 'aria-controls': "#navbarTogglerDemo01", 'aria-expanded': 'false' }).childs(
        h.span({ class: "navbar-toggler-icon" }),
      ),
      h.div({ class: 'collapse navbar-collapse', id: 'navbarTogglerDemo01' }).childs(
        h.ul({ class: "navbar-nav mr-auto mt-2 mt-lg-0" }).childs(
          h.li({ class: "nav-item" }, h.a({ class: 'nav-link', href: '#' }, 'Home')),
          h.li({ class: "nav-item" }, h.a({ class: 'nav-link', href: '#' }, 'Seasons')),
        ),
      ),
    ),
  );

  const footer = h.footer({ class: classes.footer }).childs(
    h.div({ class: classes.container + ' container' }).childs(
      h.span({ class: 'text-muted' }).childs('theoffice-tv.online'),
    ),
  )
  
  return h.div<Model, Action>({ class: classes.root }).childs(
    header,
    discriminate<Model, 'page', 'route', 'tag'>('page', 'route', 'tag')({
      Home: Home.view(db).dimap((m: Jet<Model>) => m.key('page', 'model'), action => ({ tag: '@Children', action, update: routeToComponent['Home'].update })),
      Episode: Episode.view(db).dimap((m: Jet<Model>) => m.key('page', 'model'), action => ({ tag: '@Children', action, update: routeToComponent['Episode'].update })),
      Season: Season.view(db).dimap((m: Jet<Model>) => m.key('page', 'model'), action => ({ tag: '@Children', action, update: routeToComponent['Season'].update })),
    }),
    footer,
  );
}

// Styles
function styles() {
  const unit = 8;
  const gridPadding = unit * 2;
  const columns = 3;
  const maxWidth = 840;
  
  return {
    root: {
    },

    content: {
      maxWidth: maxWidth + (gridPadding * (columns - 1)),
      margin: [0, 'auto'],
    },

    seasonsUl: {
      padding: 0,
      display: 'flex',
      justifyContent: 'space-between',
      flexWrap: 'wrap',
    },
    
    seasonLi: {
      width: maxWidth / columns,
      listStyle: 'none',
      margin: [unit * 2, 0, 0, 0],
      '& img': { width: '100%' },
    },

    episodes: {
      fontSize: 14,
      maxHeight: 22,
      overflow: 'hidden',
    },

    footer: {
      position: 'absolute',
      bottom: 0,
      width: '100%',
      height: 60,
      lineHeight: '60px',
      backgroundColor: '#f5f5f5',
    },
    
    container: {
      width: 'auto',
      maxWidth: maxWidth + (gridPadding * (columns - 1)),
      padding: [0, unit],
      boxSizing: 'content-box',
    },
    
    '@global': {
      html: {
        position: 'relative',
        minHeight: '100%',
      },
      body: {
        marginBottom: 60,
      },
    },
  };
}

const { classes } = jss.createStyleSheet(styles()).attach();


function dispatch(action: Action) {
  const rawPatch = update(action, model);
  const patch = preparePatch(model, rawPatch);
  el = actuate(el, sdom, new Jet(model, patch));
  model = applyPatch(model, patch);
  console.log('patch', patch);
  console.log('model', model);
}

const defaultRoute: Route = { tag: 'Home' };
const route = parse(location) || defaultRoute;
const pageModel = routeToComponent[route.tag].init(db, route);
let model: Model = { page: { route, model: pageModel } };
const container = document.createElement('div');
document.body.appendChild(container);
const sdom = view(db).map(dispatch);
let el = create(sdom, model);


window.onpopstate = function(event) {
  const route = parse(location) || defaultRoute;
  dispatch({ tag: 'RouteChange', value: route });
};
container.appendChild(el);


declare const require: any;
