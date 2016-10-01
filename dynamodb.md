# dynamoDB

## withValueMap
大部分的`Expression`中需要的`value`必须经过`ValueMap`才可以使用
e.g.
```
UpdateItemSpec updateItemSpec = new UpdateItemSpec()
 .withPrimaryKey(primaryKey)
 .setUpdateExpression("set name=:v_name")
 .withValueMap(new ValueMap()
            .withString(":v_name", "xiaoMing"))
 .withReturnValues(ReturnValue.ALL_NEW);
```
`setUpdateExpression`中`set name="xiaoMing"`是不可行的, 必须通过ValueMap.

**note:**有的会内部调用withValueMap，这样手动调用会引起冲突。
e.g.
```Java
UpdateItemSpec updateItemSpec = new UpdateItemSpec()
    .withPrimaryKey(primaryKey)
    .withConditionExpression(conditionExpression)
    .withExpressionSpec(expressionSpec)
    .withValueMap(valueMap)
    .withReturnValues(ReturnValue.ALL_NEW);
```
`withExpressionSpec`会在内部调用`withValueMap`造成很难调试的问题