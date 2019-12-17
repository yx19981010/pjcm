package com.samsph.pjcm.vo;

import io.swagger.annotations.ApiModel;
import lombok.Data;
import lombok.EqualsAndHashCode;

/**
 * @author hujiahao
 */

@Data
@EqualsAndHashCode(callSuper = true)
@ApiModel(description = "稿件VO-审稿人")
public class Post4RevVO  extends  Post4RevSimpleVO{
}
