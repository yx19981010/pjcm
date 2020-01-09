package com.samsph.pjcm.controller;

import com.samsph.pjcm.config.PageData;
import com.samsph.pjcm.config.constant.Field;
import com.samsph.pjcm.config.constant.RoleType;
import com.samsph.pjcm.config.exception.AjaxResponse;
import com.samsph.pjcm.config.exception.CustomException;
import com.samsph.pjcm.config.exception.CustomExceptionType;
import com.samsph.pjcm.model.ReviewerField;
import com.samsph.pjcm.model.User;
import com.samsph.pjcm.service.ReviewerFieldService;
import com.samsph.pjcm.service.UserRoleService;
import com.samsph.pjcm.service.UserService;
import com.samsph.pjcm.vo.*;
import io.swagger.annotations.Api;
import io.swagger.annotations.ApiImplicitParam;
import io.swagger.annotations.ApiImplicitParams;
import io.swagger.annotations.ApiOperation;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.PageRequest;
import org.springframework.security.access.prepost.PreAuthorize;
import org.springframework.validation.annotation.Validated;
import org.springframework.web.bind.annotation.*;

import javax.validation.Valid;
import javax.validation.constraints.Max;
import javax.validation.constraints.Min;
import javax.validation.constraints.NotNull;
import java.util.ArrayList;
import java.util.List;

@Api(tags = "审稿人-领域管理")
@Validated
@RestController
@RequestMapping(value = "/api/v1")
public class ReviewerFieldController {
    @Autowired
    private ReviewerFieldService reviewerFieldService;
    @Autowired
    private UserRoleService userRoleService;
    @Autowired
    private UserService userService;

    @ApiOperation(value = "添加审稿人-领域")
    @PostMapping("/reviewerFields")
    @PreAuthorize("hasAnyRole('ROLE_ADMIN')")
    public AjaxResponse addReviewerField(@Valid @RequestBody ReviewerFieldVoPost reviewerFieldVoPost){
        //检测编辑id是否正确（是否有这个uid以及该uid是否是编辑）
        if(!userRoleService.findUserHasRole(reviewerFieldVoPost.getReviewerUid(), RoleType.REVIEWER_ROLE)){
            throw new CustomException(CustomExceptionType.USER_INPUT_ERROR,"审稿人id错误或者审稿人未激活!!!");
        }else{
            List<ReviewerField> list = reviewerFieldService.findByReviewerUid(reviewerFieldVoPost.getReviewerUid());
            if(list!=null && list.size()>0){
                for(ReviewerField reviewerField : list){
                    if(reviewerField.getField().equals(reviewerFieldVoPost.getField())){
                        throw new CustomException(CustomExceptionType.USER_INPUT_ERROR,"审稿人已存在该领域");
                    }
                }
            }
            ReviewerField reviewerField = new ReviewerField();
            reviewerField.setReviewerUid(reviewerFieldVoPost.getReviewerUid());
            reviewerField.setField(reviewerFieldVoPost.getField());
            reviewerFieldService.addReviewerField(reviewerField);
            return AjaxResponse.success();
        }
    }

    @ApiOperation(value = "修改审稿人-领域")
    @PutMapping("/reviewerFields")
    @PreAuthorize("hasAnyRole('ROLE_ADMIN')")
    public AjaxResponse updateReviewerField(@Valid @RequestBody ReviewerFieldVoPut reviewerFieldVoPut){
        //权限检测 用户身份是管理员且登录
        if(!reviewerFieldService.findReviewerField(reviewerFieldVoPut.getId()).isPresent()){
            throw new CustomException(CustomExceptionType.USER_INPUT_ERROR,"id无效或未输入id!!!!");
        }else{
            ReviewerField reviewerField = reviewerFieldService.findReviewerField(reviewerFieldVoPut.getId()).get();
            if(reviewerFieldVoPut.getReviewerUid()!=null){
                if(!userRoleService.findUserHasRole(reviewerFieldVoPut.getReviewerUid(),RoleType.REVIEWER_ROLE)){
                    throw new CustomException(CustomExceptionType.USER_INPUT_ERROR,"审稿人id错误或者审稿人未激活!!!");
                }else{
                    reviewerField.setReviewerUid(reviewerFieldVoPut.getReviewerUid());
                }
            }
            if(reviewerFieldVoPut.getField()!=null){
                reviewerField.setField(reviewerFieldVoPut.getField());
            }
            reviewerFieldService.updateReviewerField(reviewerField);
            return AjaxResponse.success();
        }
    }

    @ApiOperation(value = "删除审稿人-领域")
    @DeleteMapping("/reviewerFields/id={id}")
    @PreAuthorize("hasAnyRole('ROLE_ADMIN')")
    public AjaxResponse deleteReviewerField(@NotNull(message = "id不能为空")@Min(value = 1,message = "id必须是正整数") @PathVariable Integer id){
        //权限检测TODO 用户身份是管理员且登录
        if(!reviewerFieldService.findReviewerField(id).isPresent()){
            throw new CustomException(CustomExceptionType.USER_INPUT_ERROR,"id无效或未输入id!!!!");
        }else{
            reviewerFieldService.deleteReviewerField(id);
            return AjaxResponse.success();
        }
    }

    @ApiOperation(value = "得到所有审稿人-领域信息")
    @GetMapping("/reviewerFields/all")
    @PreAuthorize("hasAnyRole('ROLE_ADMIN','ROLE_EDITOR')")
    public AjaxResponse findAllReviewerFields(){

        return AjaxResponse.success(reviewerFieldService.findAll());
    }

    @ApiOperation(value = "通过领域id得到审稿人-领域信息")
    @ApiImplicitParams({
            @ApiImplicitParam(name="fieldId",value="领域id"),
            @ApiImplicitParam(name="page",value="页数"),
            @ApiImplicitParam(name="size",value="大小")
    })
    @GetMapping("/reviewerFields")
    @PreAuthorize("hasAnyRole('ROLE_ADMIN','ROLE_EDITOR')")
    public AjaxResponse findReviewerFields(@NotNull(message = "id不能为空") @Min(value = Field.LEAST_FIELD,message = "领域id最小为"+Field.LEAST_FIELD) @Max(value = Field.TOTAL_FIELD,message = "领域id最大为"+Field.TOTAL_FIELD) @RequestParam("fieldId") Integer fieldId,
                                           @NotNull(message = "未传入页数")@Min(value = 1,message = "页数必须是正整数") @RequestParam("page") Integer page,
                                           @NotNull(message = "未传入每页的大小")@Min(value = 1,message = "每页的大小必须是正整数") @RequestParam("size") Integer size){
        PageRequest pageRequest = PageRequest.of(page-1,size);
        Page<ReviewerField> reviewerFieldPage = reviewerFieldService.findReviewerFieldsByFieldId(fieldId, pageRequest);
        List<ReviewerField> reviewerFields = reviewerFieldPage.getContent();
        List<ReviewerFieldVoGetReviewer> reviewerFieldVoGetReviewers =  new ArrayList<>();
        for(ReviewerField reviewerField : reviewerFields){
            User user = userService.findUserByUid(reviewerField.getReviewerUid()).get();
            ReviewerFieldVoGetReviewer reviewerFieldVoGetReviewer = new ReviewerFieldVoGetReviewer();
            reviewerFieldVoGetReviewer.setId(reviewerField.getId());
            reviewerFieldVoGetReviewer.setReviewerUid(reviewerField.getReviewerUid());
            reviewerFieldVoGetReviewer.setEmail(user.getEmail());
            reviewerFieldVoGetReviewer.setUserName(user.getUserName());
            reviewerFieldVoGetReviewers.add(reviewerFieldVoGetReviewer);
        }
        PageData pageData = new PageData(reviewerFieldPage.getTotalPages(), (int) reviewerFieldPage.getTotalElements(), page, reviewerFieldVoGetReviewers.size(), reviewerFieldVoGetReviewers);
        return AjaxResponse.success(pageData);
    }
}
