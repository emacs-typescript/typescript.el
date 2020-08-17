const arr =
    [ 'one'
    , 'two'
    , 'three' ];

const obj =
    { a: 111
    , b: 222
    , c: 333 };

const depth = { arr: [ 'one'
                     , 'two'
                     , 'three' ]
              , obj: { a: 111
                     , b: 222
                     , c: 333 }
              , func_call: Object.assign
              ( {}
              , { x: 1
                , y: 2
                , z: 3 }
              , { i: 'i', j: 'j' } ) };
