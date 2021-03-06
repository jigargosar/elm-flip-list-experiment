import 'tachyons'
import 'tachyons/css/tachyons.css'

import './index.css'
import { Elm } from './Main.elm'
// import { Elm } from './elm.min'
import {
  forEachObjIndexed,
  isNil,
  mapObjIndexed,
  path, pick,
  propOr,
} from 'ramda'
import { identity } from 'fp-ts'

const storageKey = 'elm-flip-list-cache'
const app = Elm.Main.init({
  flags: {
    cache: JSON.parse(localStorage.getItem(storageKey) || 'null'),
  },
})

const pubs = initPubs({
  onGotClientBoundingRects: identity,
})

initSubs({
  getClientBoundingRects: ({ id, from, to }) => {
    requestAnimationFrame(() => {
      const getIdRects = idList =>
        idList.map(([fst, domId]) => {
          const el = document.getElementById(domId)
          const rect = {
            ...pick(['x', 'y', 'width', 'height'])(
              el.getBoundingClientRect(),
            ),
            ...pick(['offsetLeft', 'offsetTop'])(el),
          }
          console.debug('rect', rect)
          return [fst, rect]
        })
      const response = { id, from: getIdRects(from), to: getIdRects(to) }
      console.debug('onGotClientBoundingRects', response)
      pubs.onGotClientBoundingRects(response)
    })
  },
  localStorageSetJsonItem: ([k, v]) => {
    console.groupCollapsed('localStorageSetJsonItem', k)
    console.log(v)
    console.groupEnd()
    localStorage.setItem(k, JSON.stringify(v))
  },
  setCache: cache => {
    if (isNil(cache)) {
      localStorage.removeItem(storageKey)
    } else {
      localStorage.setItem(storageKey, JSON.stringify(cache))
    }
  },
})

function initSubs(subs) {
  forEachObjIndexed((listener, portName) => {
    const subscribe = path(['ports', portName, 'subscribe'])(app)
    if (!subscribe) {
      console.warn('Subscribe: Port Not Found:', portName)
      return
    }
    console.debug('Subscribe: Port Handler Attached', portName)
    subscribe(listener)
  })(subs)
  const ports = propOr({}, 'ports')(app)
  forEachObjIndexed((port, portName) => {
    if (port.subscribe && !subs[portName]) {
      console.warn('Subscribe: Port Handler Missing', portName)
    }
  })(ports)
}

function initPubs(pubs) {
  return mapObjIndexed((fn, portName) => {
    return arg => {
      const send = path(['ports', portName, 'send'])(app)
      if (!send) {
        console.warn('Send: Port Not Found:', portName, arg)
        return
      }
      if (send) {
        send(arg)
      }
    }
  })(pubs)
}
