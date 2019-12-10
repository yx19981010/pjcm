package com.samsph.pjcm.web.controller;

import com.samsph.pjcm.config.PageData;
import com.samsph.pjcm.config.auth.CurrentUser;
import com.samsph.pjcm.config.constant.Field;
import com.samsph.pjcm.config.constant.RoleType;
import com.samsph.pjcm.config.exception.AjaxResponse;
import com.samsph.pjcm.config.exception.CustomException;
import com.samsph.pjcm.config.exception.CustomExceptionType;
import com.samsph.pjcm.model.EditorField;
import com.samsph.pjcm.model.ReviewerField;
import com.samsph.pjcm.service.ReviewerFieldService;
import com.samsph.pjcm.service.UserRoleService;
import com.samsph.pjcm.config.utils.DozerUtil;
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
    private CurrentUser currentUser;

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
                    if(reviewerField.getField() == reviewerFieldVoPost.getField().getCode()){
                        throw new CustomException(CustomExceptionType.USER_INPUT_ERROR,"审稿人已存在该领域");
                    }
                }
            }
            ReviewerField reviewerField = new ReviewerField();
            reviewerField.setReviewerUid(reviewerFieldVoPost.getReviewerUid());
            reviewerField.setField(reviewerFieldVoPost.getField().getCode());
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

    @ApiOperation(value = "得到审稿人-领域列表")
    @ApiImplicitParams({
            @ApiImplicitParam(name="reviewerOrFieldId",value="审稿人id或领域id"),
            @ApiImplicitParam(name="flag",value="标识，1代表查看该审稿人所属的领域，2代表查看该领域下的所有审稿人"),
            @ApiImplicitParam(name="page",value="页数"),
            @ApiImplicitParam(name="size",value="大小")
    })
    @GetMapping("/reviewerFields")
    @PreAuthorize("hasAnyRole('ROLE_ADMIN','ROLE_EDITOR','ROLE_REVIEWER')")
    public AjaxResponse findReviewerFields(@NotNull(message = "id不能为空")@Min(value = 1,message = "审稿人或领域id必须是正整数") @RequestParam("reviewerOrFieldId") Integer reviewerOrFieldId,
                                           @NotNull(message = "flag不能为空")@Min(value = 1,message = "flag最小值为1")@Max(value = 2,message = "flag最大值为2") @RequestParam("flag") Integer flag,
                                           @NotNull(message = "未传入页数")@Min(value = 1,message = "页数必须是正整数") @RequestParam("page") Integer page,
                                           @NotNull(message = "未传入每页的大小")@Min(value = 1,message = "每页的大小必须是正整数") @RequestParam("size") Integer size){
        PageRequest pageRequest = PageRequest.of(page-1,size);
        if(flag == 1){
//            if(currentUser.getCurrentUser().getUserRole() == RoleType.REVIEWER_ROLE){
//                if(currentUser.getCurrentUser().getUserId() != reviewerOrFieldId){
//                    throw new CustomException(CustomExceptionType.USER_INPUT_ERROR,"无权查看其他审稿人的领域!!!");
//                }
//            }
            if(!userRoleService.findUserHasRole(reviewerOrFieldId,RoleType.REVIEWER_ROLE)){
                throw new CustomException(CustomExceptionType.USER_INPUT_ERROR,"审稿人id错误或者审稿人未激活!!!");
            }else {
                Page<ReviewerField> reviewerFieldPage = reviewerFieldService.findReviewerFieldsByReviewerUid(reviewerOrFieldId, pageRequest);
                List<ReviewerField> reviewerFields = reviewerFieldPage.getContent();
                List<ReviewerFieldVoGetField> reviewerFieldVoGetFields = DozerUtil.mapList(reviewerFields, ReviewerFieldVoGetField.class);
                PageData pageData = new PageData(reviewerFieldPage.getTotalPages(), (int) reviewerFieldPage.getTotalElements(), page, reviewerFieldVoGetFields.size(), reviewerFieldVoGetFields);
                return AjaxResponse.success(pageData);
            }
        }else{
//            if(currentUser.getCurrentUser().getUserRole() == RoleType.REVIEWER_ROLE){
//                throw new CustomException(CustomExceptionType.USER_INPUT_ERROR,"无权查看该领域下的审稿人!!!");
//            }
            if(reviewerOrFieldId > Field.TOTAL_FIELD){
                throw new CustomException(CustomExceptionType.USER_INPUT_ERROR,"领域id错误!!!");
            }else {
                Page<ReviewerField> reviewerFieldPage = reviewerFieldService.findReviewerFieldsByFieldId(reviewerOrFieldId, pageRequest);
                List<ReviewerField> reviewerFields = reviewerFieldPage.getContent();
                List<ReviewerFieldVoGetReviewer> reviewerFieldVoGetReviewers = DozerUtil.mapList(reviewerFields, ReviewerFieldVoGetReviewer.class);
                PageData pageData = new PageData(reviewerFieldPage.getTotalPages(), (int) reviewerFieldPage.getTotalElements(), page, reviewerFieldVoGetReviewers.size(), reviewerFieldVoGetReviewers);
                return AjaxResponse.success(pageData);
            }
        }
    }
}
