import { SDOM, h, create } from 'typescript-sdom';
import db, { Db, Season } from './db';
import jss from 'jss';
import preset from 'jss-preset-default';
import * as r from 'typescript-invertible-router';
jss.setup(preset());

r.oneOf();

// Model
export type Model = {
};

// Action
export type Action =
  | { }
;

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

  const content = h.section({ class: classes.content }).childs(
    h.ul({ class: classes.seasonsUl }).childs(...db.seasons.map(renderSeason)),
  );

  const footer = h.footer({ class: classes.footer }).childs(
    h.div({ class: classes.container + ' container'  }).childs(
      h.span({ class: 'text-muted' }).childs('theoffice-tv.online'),
    ),
  )

  function renderSeason(season: Season) {
    return h.li({ class: classes.seasonLi }).childs(
      h('h3', season.code),
      h.a({ href: season.href }).childs(h.img({ class: 'rounded', src: season.thumbnail })),
      h.div({ style: 'overflow: hidden' }).childs(...season.series.map(seria => h.span(h.a({ href: seria.href }).childs(seria.code.replace(/^S0\d/, '')), ' '))),
    );
  }  
  
  return h.div<Model, Action>({ class: classes.root }).childs(
    header,
    content,
    footer,
  );
}

// Styles
function styles() {
  const unit = 8;
  return {
    root: {
      
    },

    content: {
      maxWidth: 900 + (unit * 2),
      margin: [0, 'auto'],
    },

    seasonsUl: {
      padding: 0,
      display: 'flex',
      justifyContent: 'space-between',
      flexWrap: 'wrap',
    },
    
    seasonLi: {
      width: 900 / 3,
      listStyle: 'none',
      margin: [unit * 2, 0, 0, 0],
      '& img': { width: '100%' },
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
      maxWidth: 900 + unit * 2,
      padding: [0, unit],
      boxSizing: 'content-box',
    },
    
    '@global': {
      html: {
        position: 'relative',
        minheight: '100%',
      },
      body: {
        marginBottom: 60,
      }
    },
  };
}

const { classes } = jss.createStyleSheet(styles()).attach();

let model: Model = { };
const container = document.createElement('div');
document.body.appendChild(container);
const getModel = () => model;
const sdom = view(db);
const el = create(sdom, getModel);
window.onpopstate = function(event) {
//  handleAction({ tag: 'HashChange', hash: location.hash });
};
container.appendChild(el);
