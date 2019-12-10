package com.samsph.pjcm.web.controller;

import com.samsph.pjcm.config.constant.FieldTotal;
import com.samsph.pjcm.config.PageData;
import com.samsph.pjcm.config.constant.RoleType;
import com.samsph.pjcm.config.exception.AjaxResponse;
import com.samsph.pjcm.config.exception.CustomException;
import com.samsph.pjcm.config.exception.CustomExceptionType;
import com.samsph.pjcm.model.ReviewerField;
import com.samsph.pjcm.service.ReviewerFieldService;
import com.samsph.pjcm.service.UserRoleService;
import com.samsph.pjcm.config.utils.DozerUtil;
import com.samsph.pjcm.vo.*;
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
import java.util.List;

@Validated
@RestController
@RequestMapping(value = "/api/v1")
public class ReviewerFieldController {
    @Autowired
    private ReviewerFieldService reviewerFieldService;
    @Autowired
    private UserRoleService userRoleService;

    @PostMapping("/reviewerfields")
    @PreAuthorize("hasAnyRole('ROLE_1')")
    public AjaxResponse addReviewerField(@Valid @RequestBody ReviewerFieldVoPost reviewerFieldVoPost){
        //检测编辑id是否正确（是否有这个uid以及该uid是否是编辑）
        if(!userRoleService.findUserHasRole(reviewerFieldVoPost.getReviewerUid(), RoleType.REVIEWER_ROLE)){
            throw new CustomException(CustomExceptionType.USER_INPUT_ERROR,"审稿人id错误或者审稿人未激活!!!");
        }else{
            ReviewerField reviewerField = new ReviewerField();
            reviewerField.setReviewerUid(reviewerFieldVoPost.getReviewerUid());
            reviewerField.setField(reviewerFieldVoPost.getField());
            reviewerFieldService.addReviewerField(reviewerField);
            return AjaxResponse.success();
        }
    }

    @PutMapping("/reviewerfields")
    @PreAuthorize("hasAnyRole('ROLE_1')")
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

    @DeleteMapping("/reviewerfields/id={id}")
    @PreAuthorize("hasAnyRole('ROLE_1')")
    public AjaxResponse deleteReviewerField(@NotNull(message = "id不能为空")@Min(value = 1,message = "id必须是正整数") @PathVariable Integer id){
        //权限检测TODO 用户身份是管理员且登录
        if(!reviewerFieldService.findReviewerField(id).isPresent()){
            throw new CustomException(CustomExceptionType.USER_INPUT_ERROR,"id无效或未输入id!!!!");
        }else{
            reviewerFieldService.deleteReviewerField(id);
            return AjaxResponse.success();
        }
    }

    @GetMapping("/reviewerfields")
    @PreAuthorize("hasAnyRole('ROLE_1','ROLE_2')")
    public AjaxResponse findReviewerFields(@NotNull(message = "id不能为空")@Min(value = 1,message = "审稿人id必须是正整数") @RequestParam("reviewerOrfeildId") Integer reviewerOrfeildId,
                                           @NotNull(message = "flag不能为空")@Min(value = 1,message = "flag最小值为1")@Max(value = 2,message = "flag最大值为2") @RequestParam("flag") Integer flag,
                                           @NotNull(message = "未传入页数")@Min(value = 1,message = "页数必须是正整数") @RequestParam("page") Integer page,
                                           @NotNull(message = "未传入每页的大小")@Min(value = 1,message = "每页的大小必须是正整数") @RequestParam("size") Integer size){
        PageRequest pageRequest = PageRequest.of(page-1,size);
        if(flag == 1){
            if(!userRoleService.findUserHasRole(reviewerOrfeildId,RoleType.REVIEWER_ROLE)){
                throw new CustomException(CustomExceptionType.USER_INPUT_ERROR,"审稿人id错误或者审稿人未激活!!!");
            }else {
                Page<ReviewerField> reviewerFieldPage = reviewerFieldService.findReviewerFieldsByReviewerUid(reviewerOrfeildId, pageRequest);
                List<ReviewerField> reviewerFields = reviewerFieldPage.getContent();
                List<ReviewerFieldVoGetField> reviewerFieldVoGetFields = DozerUtil.mapList(reviewerFields, ReviewerFieldVoGetField.class);
                PageData pageData = new PageData(reviewerFieldPage.getTotalPages(), (int) reviewerFieldPage.getTotalElements(), page, reviewerFieldVoGetFields.size(), reviewerFieldVoGetFields);
                return AjaxResponse.success(pageData);
            }
        }else{
            if(reviewerOrfeildId > FieldTotal.TOTAL_FIELD){
                throw new CustomException(CustomExceptionType.USER_INPUT_ERROR,"领域id错误!!!");
            }else {
                Page<ReviewerField> reviewerFieldPage = reviewerFieldService.findReviewerFieldsByFieldId(reviewerOrfeildId, pageRequest);
                List<ReviewerField> reviewerFields = reviewerFieldPage.getContent();
                List<ReviewerFieldVoGetReviewer> reviewerFieldVoGetReviewers = DozerUtil.mapList(reviewerFields, ReviewerFieldVoGetReviewer.class);
                PageData pageData = new PageData(reviewerFieldPage.getTotalPages(), (int) reviewerFieldPage.getTotalElements(), page, reviewerFieldVoGetReviewers.size(), reviewerFieldVoGetReviewers);
                return AjaxResponse.success(pageData);
            }
        }
    }
}
