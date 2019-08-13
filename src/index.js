import './index.css'
import { Elm } from './Main.elm'
// import { Elm } from './elm.min'
import { Fire } from './fire'
import {
  mapObjIndexed,
  identity,
  propOr,
  forEachObjIndexed,
  path,
  isNil,
} from 'ramda'

const cachedProjectList = JSON.parse(
  localStorage.getItem('cachedProjectList') || 'null',
)

console.log('cachedProjectList', cachedProjectList)

const storageKey = 'appCache'
const app = Elm.Main.init({
  flags: {
    cachedTodoList: JSON.parse(
      localStorage.getItem('cachedTodoList') || 'null',
    ),
    cachedProjectList: cachedProjectList,
    cachedAuthState: JSON.parse(
      localStorage.getItem('cachedAuthState') || 'null',
    ),
    browserSize: { width: window.innerWidth, height: window.innerHeight },
    cache: JSON.parse(localStorage.getItem(storageKey) || 'null'),
  },
})
const fire = Fire()

const pubs = initPubs({
  onAuthStateChanged: identity,
  onFirestoreQueryResponse: identity,
})

fire.onAuthStateChanged(pubs.onAuthStateChanged)

initSubs({
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
  signIn: () => fire.signIn(),
  signOut: () => fire.signOut(),
  changeTodoTitle: async todoId => {
    const faker = await import('faker')
    const todoCRef = fire.userCRef('todos')
    await todoCRef
      .doc(todoId)
      .update({ title: faker.hacker.phrase(), modifiedAt: Date.now() })
  },
  // persistTodoList: async todoList => {
  //   const todoCRef = fire.userCRef('todos')
  //   const ps = todoList.map(todo => {
  //     return todoCRef.doc(todo.id).set(todo, { merge: false })
  //   })
  //   await Promise.all(ps)
  // },
  queryFirestore: async options => {
    const cRef = fire.userCRef(options.userCollectionName)
    const query = options.whereClause.reduce(
      (query, [fieldPath, op, value]) => {
        return query.where(fieldPath, op, value)
      },
      cRef,
    )
    fire.addDisposerWithId(
      options.id,
      query.onSnapshot(qs => {
        const docDataList = qs.docs.map(ds => ds.data())
        const response = { id: options.id, docDataList }
        console.groupCollapsed('onFirestoreQueryResponse', options)
        console.log(docDataList)
        console.groupEnd()
        pubs.onFirestoreQueryResponse(response)
      }),
    )
  },
  disposeFirestoreQuery: id => {
    fire.disposeNamed(id)
  },
  updateFirestoreDoc: options => {
    const doc = fire.userDocRef(options.userDocPath)
    return doc.update(options.data)
  },
  deleteFirestoreDoc: options => {
    const doc = fire.userDocRef(options.userDocPath)
    return doc.delete()
  },
  addFirestoreDoc: async options => {
    const faker = await import('faker')
    const cRef = fire.userCRef(options.userCollectionName)

    const docRef = cRef.doc()

    const data = Object.assign(
      {},
      options.data,
      { id: docRef.id },
      options.data.title === '' && options.userCollectionName === 'todos'
        ? { title: faker.hacker.phrase() }
        : {},

      options.data.title === '' &&
        options.userCollectionName === 'projects'
        ? { title: `${faker.hacker.ingverb()} ${faker.hacker.noun()}` }
        : {},
    )
    return docRef.set(data)
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
