package com.samsph.pjcm.controller;

import cn.hutool.core.bean.BeanUtil;
import cn.hutool.core.bean.copier.CopyOptions;
import com.samsph.pjcm.config.PageDataT;
import com.samsph.pjcm.config.constant.*;
import com.samsph.pjcm.config.exception.AjaxResponse;
import com.samsph.pjcm.config.exception.CustomException;
import com.samsph.pjcm.config.exception.CustomExceptionType;
import com.samsph.pjcm.config.utils.DozerUtil;
import com.samsph.pjcm.config.utils.FileUtil;
import com.samsph.pjcm.model.Post;
import com.samsph.pjcm.model.PostReviewer;
import com.samsph.pjcm.model.User;
import com.samsph.pjcm.query.*;
import com.samsph.pjcm.service.*;
import com.samsph.pjcm.vo.*;
import io.swagger.annotations.Api;
import io.swagger.annotations.ApiImplicitParam;
import io.swagger.annotations.ApiImplicitParams;
import io.swagger.annotations.ApiOperation;
import org.apache.commons.lang3.StringUtils;
import org.dozer.Mapper;
import org.springframework.beans.propertyeditors.CustomDateEditor;
import org.springframework.data.domain.Page;
import org.springframework.security.access.prepost.PreAuthorize;
import org.springframework.validation.annotation.Validated;
import org.springframework.web.bind.WebDataBinder;
import org.springframework.web.bind.annotation.*;


import javax.annotation.Resource;
import javax.validation.constraints.NotNull;

import java.text.SimpleDateFormat;
import java.util.Date;
import java.util.List;
import java.util.Optional;

import static com.samsph.pjcm.config.DevUserId.*;
import static com.samsph.pjcm.config.constant.Genre.*;


/**
 * @author hujiahao
 */

@Validated
@RestController
@RequestMapping("/api/v1/posts")
@Api(tags = "2. 稿件管理")
public class PostController {


    @Resource
    private Mapper dozerMapper;

    @Resource
    PostService postService;

    @Resource
    MailService mailService;

    @Resource
    PostReviewerService postReviewerService;

    @Resource
    UserService userService;


    @Resource
    private EditorFieldService editorFieldService;

    @InitBinder
    public void initBinder(WebDataBinder binder) {
        binder.registerCustomEditor(Date.class,
                new CustomDateEditor(new SimpleDateFormat("yyyy-MM-dd"), true, 10));
    }

    @PostMapping()
    @PreAuthorize("hasAnyRole('ROLE_CONTRIBUTOR')")
    @ApiOperation(value = "投稿人创建稿件记录")
    @ApiImplicitParam(name = "postQuery",
            value = "必填：field、title、genre、fundLevel、writersInfo\n" +
                    "不填：id\n" +
                    "备注：前端需确定中英文关键词和作者信息的格式\n" +
                    "TODO：服务器端对中英文关键词和作者信息做格式校验",
            dataType = "PostQuery")
    public AjaxResponse savePost(@Validated({Add.class}) @RequestBody PostQuery postQuery) {

        // TODO: 检查关键字和作者信息格式

//        int uid = currentUser.getCurrentUser().getUserId();
        int uid = CONTRIBUTOR_ID;
//        int size = editorFieldService.findByFieldId(postQuery.getField().getCode()).size();
//        if (size == 0) {
//            throw new CustomException(CustomExceptionType.SYSTEM_ERROR, "该领域下无编辑");
//        }
        Post post = postService.savePost(postQuery, uid);

        Post4CtrSimpleVO post4CtrSimpleVO = dozerMapper.map(post, Post4CtrSimpleVO.class);

        return AjaxResponse.success(post4CtrSimpleVO);
    }

    @GetMapping("{id}/role=ctr")
    @PreAuthorize("hasAnyRole('ROLE_CONTRIBUTOR')")
    @ApiOperation(value = "投稿人根据id获取自己的稿件")
    public AjaxResponse getPost1(@NotNull @PathVariable Integer id) {
        Post post = postService.getPost(id);

        // int uid = currentUser.getCurrentUser().getUserId();
        int uid = CONTRIBUTOR_ID;

        // 检查其为稿件投稿人
        if (uid != post.getContributorUid()) {
            throw new CustomException(CustomExceptionType.USER_INPUT_ERROR, ErrMsg.NOT_CONTRIBUTOR);
        }

        return AjaxResponse.success(dozerMapper.map(post, Post4CtrVO.class));
    }

    @PutMapping("type=1")
    @PreAuthorize("hasAnyRole('ROLE_CONTRIBUTOR')")
    @ApiOperation(value = "稿件状态为待提交，投稿人修改稿件基本信息")
    @ApiImplicitParam(name = "postQuery", value = "必填id，其余选填", dataType = "PostQuery")
    public AjaxResponse updatePost1(@Validated({Update.class}) @RequestBody PostQuery postQuery) {
        int pid = postQuery.getId();
        Post post = postService.getPost(pid);

//        int uid = currentUser.getCurrentUser().getUserId();
        int uid = CONTRIBUTOR_ID;

        // 检查其为稿件投稿人
        if (uid != post.getContributorUid()) {
            throw new CustomException(CustomExceptionType.USER_INPUT_ERROR, ErrMsg.NOT_CONTRIBUTOR);
        }

        // 检查稿件状态
        if (post.getStatus() != PostStatus.TO_BE_SUBMITTED.getCode()) {
            throw new CustomException(CustomExceptionType.USER_INPUT_ERROR, ErrMsg.WRONG_STATUS);
        }

        // TODO: 检查关键字和作者信息格式

        // 更新稿件基本信息
        BeanUtil.copyProperties(postQuery, post,
                CopyOptions.create().setIgnoreNullValue(true).setIgnoreError(true));
        postService.updatePost(post);

        return AjaxResponse.success();
    }

    @PutMapping("{id}/type=submit")
    @PreAuthorize("hasAnyRole('ROLE_CONTRIBUTOR')")
    @ApiOperation(value = "稿件状态为待提交，投稿人提交以待初审")
    public AjaxResponse updatePost2(@NotNull(message = "id不能为空") @PathVariable Integer id) {
        Post post = postService.getPost(id);

//        int uid = currentUser.getCurrentUser().getUserId();
        int uid = CONTRIBUTOR_ID;

        // 检查其为稿件投稿人
        if (uid != post.getContributorUid()) {
            throw new CustomException(CustomExceptionType.USER_INPUT_ERROR, ErrMsg.NOT_CONTRIBUTOR);
        }

        // 检查稿件状态
        if (post.getStatus() != PostStatus.TO_BE_SUBMITTED.getCode()) {
            throw new CustomException(CustomExceptionType.USER_INPUT_ERROR, ErrMsg.WRONG_STATUS);
        }

        // 检查稿件信息填写完整，能提交初审
        checkPostCanSubmit(post);

//        int size = editorFieldService.findByFieldId(post.getField()).size();
//        if (size == 0) {
//            throw new CustomException(CustomExceptionType.SYSTEM_ERROR, "该领域下无编辑");
//        }
//        int num = (int) (1 + Math.random() * (size - 1 + 1));

//        int editorId = editorFieldService.findByFieldId(post.getField()).get(num - 1).getEditorUid();
        int editorId = 8;

        post.setEditorUid(editorId);
        post.setStatus(PostStatus.PENDING_FIRST_EXAM.getCode());
        post.setSubmitTime((new Date()));
        postService.updatePost(post);

        return AjaxResponse.success();
    }

    @GetMapping("/{id}/role=ed")
    @PreAuthorize("hasAnyRole('ROLE_EDITOR')")
    @ApiOperation(value = "编辑根据id获取负责编辑的稿件")
    public AjaxResponse getPost2(@NotNull(message = "id不能为空") @PathVariable Integer id) {
        Post post = postService.getPost(id);

        //        int uid = currentUser.getCurrentUser().getUserId();
        int uid = EDITOR_ID;

        // 检查其为稿件编辑
        if (uid != post.getEditorUid()) {
            throw new CustomException(CustomExceptionType.USER_INPUT_ERROR, ErrMsg.NOT_EDITOR);
        }

        Post4EdVO retData = dozerMapper.map(post, Post4EdVO.class);
        retData.setContributor(getName(post.getContributorUid()));
        return AjaxResponse.success(retData);
    }

    @PutMapping("type=examFirst")
    @PreAuthorize("hasAnyRole('ROLE_EDITOR')")
    @ApiOperation(value = "稿件状态为待初审，编辑进行初审")
    public AjaxResponse updatePost3(@Validated @RequestBody EditorAuditQuery editorAuditQuery) {
        //        int uid = currentUser.getCurrentUser().getUserId();
        int uid = EDITOR_ID;

        // 获得稿件
        Post post = postService.getPost(editorAuditQuery.getId());

        // 检查操作者为该稿件编辑
        if (uid != post.getEditorUid()) {
            throw new CustomException(CustomExceptionType.USER_INPUT_ERROR, ErrMsg.NOT_EDITOR);
        }

        // 检查稿件状态
        if (post.getStatus() != PostStatus.PENDING_FIRST_EXAM.getCode()) {
            throw new CustomException(CustomExceptionType.USER_INPUT_ERROR, ErrMsg.WRONG_STATUS);
        }

        String comment = editorAuditQuery.getComment();
        if (editorAuditQuery.getPass()) {
            // 如果初审通过
            post.setStatus(PostStatus.REVIEWER_TO_BE_SELECTED.getCode());
        } else {
            // 初审不通过
            if (comment == null || StringUtils.isBlank(comment)) {
                throw new CustomException(CustomExceptionType.USER_INPUT_ERROR, ErrMsg.REJECT_COMMENT_NEEDED);
            }
            post.setStatus(PostStatus.FIRST_EXAM_REJECTED.getCode());
        }
        post.setFirstExamComment(comment);
        post.setFirstExamCommentTime(new Date());
        postService.updatePost(post);

        return AjaxResponse.success();
    }


    @GetMapping("role=rev&type=unanswer")
    @PreAuthorize("hasAnyRole('ROLE_REVIEWER')")
    @ApiOperation(value = "审稿人获取待答复的稿件列表")
    @ApiImplicitParams({
            @ApiImplicitParam(name = "number", value = "分页页号", required = true, dataType = "int"),
            @ApiImplicitParam(name = "size", value = "分页大小", required = true, dataType = "int"),
            @ApiImplicitParam(name = "ascend", value = "是否升序，默认为true", dataType = "boolean"),
            @ApiImplicitParam(name = "start", value = "开始提交日期，start与end成对使用，默认为所有时间", dataType = "date-time"),
            @ApiImplicitParam(name = "end", value = "结束提交日期，start与end成对使用，默认为所有时间", dataType = "date-time")
    })
    public AjaxResponse getAll3(@NotNull(message = "number不能为空") @RequestParam("number") Integer number,
                                @NotNull(message = "size不能为空") @RequestParam("size") Integer size,
                                @RequestParam(value = "ascend", required = false) Boolean ascend,
                                @RequestParam(value = "start", required = false) Date start,
                                @RequestParam(value = "end", required = false) Date end) {

        checkStartAndEndTime(start, end);

        // int uid = currentUser.getCurrentUser().getUserId();
        int uid = REVIEWER_ID;

        if (ascend == null) {
            ascend = true;
        }

        Page<Post> page;
        if (start == null) {
            page = postService.getAllByRevUidAndAccept(uid, MyBoolean.DEFAULT, number, size, ascend);
        } else {
            page = postService.getAllByRevUidAndAcceptAndSubmitTime(uid, MyBoolean.DEFAULT, start, end, number, size, ascend);
        }

        return AjaxResponse.success(DozerUtil.mapPage(page, Post4RevSimpleVO.class));
    }

    @GetMapping("/{id}/role=rev&type=unanswer")
    @PreAuthorize("hasAnyRole('ROLE_REVIEWER')")
    @ApiOperation(value = "审稿人根据id获取待答复的稿件")
    public AjaxResponse getPost3(@NotNull @PathVariable Integer id) {

        //        int uid = currentUser.getCurrentUser().getUserId();
        int uid = REVIEWER_ID;

        // 检查该审稿人未答复
        PostReviewer postReviewer = postReviewerService.getPostReviewer(id, uid);
        if (postReviewer.getAccept() != MyBoolean.DEFAULT.getCode()) {
            throw new CustomException(CustomExceptionType.USER_INPUT_ERROR, ErrMsg.REVIEWER_HAS_REPLIED);
        }

        Post post = postService.getPost(id);

        return AjaxResponse.success(dozerMapper.map(post, Post4RevVO.class));
    }

    @GetMapping("role=rev")
    @PreAuthorize("hasAnyRole('ROLE_REVIEWER')")
    @ApiOperation(value = "审稿人获取已接受的稿件列表")
    @ApiImplicitParams({
            @ApiImplicitParam(name = "number", value = "分页页号", required = true, dataType = "int"),
            @ApiImplicitParam(name = "size", value = "分页大小", required = true, dataType = "int"),
            @ApiImplicitParam(name = "flag", value = "审稿标识，true为需要审稿，false为审稿完成", required = true, dataType = "boolean"),
            @ApiImplicitParam(name = "ascend", value = "是否升序，默认为true", dataType = "boolean"),
            @ApiImplicitParam(name = "start", value = "开始提交日期，start与end成对使用，默认为所有时间", dataType = "date-time"),
            @ApiImplicitParam(name = "end", value = "结束提交日期，start与end成对使用，默认为所有时间", dataType = "date-time")
    })
    public AjaxResponse getAll4(@NotNull(message = "number不能为空") @RequestParam("number") Integer number,
                                @NotNull(message = "size不能为空") @RequestParam("size") Integer size,
                                @RequestParam(value = "ascend", required = false) Boolean ascend,
                                @NotNull @RequestParam(value = "flag") Boolean flag,
                                @RequestParam(value = "start", required = false) Date start,
                                @RequestParam(value = "end", required = false) Date end) {

        checkStartAndEndTime(start, end);

        //        int uid = currentUser.getCurrentUser().getUserId();
        int uid = REVIEWER_ID;

        if (ascend == null) {
            ascend = true;
        }

        Page<Post> page;

        if (start == null) {
            page = postService.getAllByRevUidAndFlag(uid, flag, number, size, ascend);
        } else {
            page = postService.getAllByRevUidAndFlagAndSubmitTime(uid, flag, start, end, number, size, ascend);
        }
        return AjaxResponse.success(DozerUtil.mapPage(page, Post4RevSimpleVO.class));
    }

    @GetMapping("/{id}/role=rev")
    @PreAuthorize("hasAnyRole('ROLE_REVIEWER')")
    @ApiOperation(value = "审稿人根据id获取负责审稿的稿件")
    public AjaxResponse getPost4(@NotNull @PathVariable Integer id) {
        //        int uid = currentUser.getCurrentUser().getUserId();
        int uid = REVIEWER_ID;

        // 检查稿件状态
        Post post = postService.getPost(id);
        if (post.getStatus() != PostStatus.FIRST_REVIEW.getCode() && post.getStatus() != PostStatus.RE_REVIEW.getCode()) {
            throw new CustomException(CustomExceptionType.USER_INPUT_ERROR, ErrMsg.WRONG_STATUS);
        }

        // 检查该审稿人已接收审稿
        PostReviewer postReviewer = postReviewerService.getPostReviewer(id, uid);
        if (postReviewer.getAccept() != MyBoolean.TRUE.getCode()) {
            throw new CustomException(CustomExceptionType.USER_INPUT_ERROR, ErrMsg.REVIEWER_HAS_REPLIED);
        }

        return AjaxResponse.success(dozerMapper.map(post, Post4RevVO.class));
    }

    @PutMapping("type=examRevise")
    @PreAuthorize("hasAnyRole('ROLE_EDITOR')")
    @ApiOperation(value = "稿件状态为待退回，编辑选择退改或退稿")
    public AjaxResponse updatePost4(@Validated @RequestBody EditorAuditQuery editorAuditQuery) {
        // int uid = currentUser.getCurrentUser().getUserId();
        int uid = EDITOR_ID;

        // 检查操作者为该稿件编辑
        Post post = postService.getPost(editorAuditQuery.getId());
        if (uid != post.getEditorUid()) {
            throw new CustomException(CustomExceptionType.USER_INPUT_ERROR, ErrMsg.NOT_EDITOR);
        }

        // 检查稿件状态
        if (post.getStatus() != PostStatus.TO_BE_RETURNED.getCode()) {
            throw new CustomException(CustomExceptionType.USER_INPUT_ERROR, ErrMsg.WRONG_STATUS);
        }

        String comment = editorAuditQuery.getComment();
        if (editorAuditQuery.getPass()) {
            // 编辑决定退回修改
            post.setStatus(PostStatus.TO_BE_REVISED.getCode());
            post.setCount(post.getCount() + 1);
            // 给投稿人发送邮件
            notifyContributor(post);

        } else {
            // 编辑退稿
            if (comment == null || StringUtils.isBlank(comment)) {
                throw new CustomException(CustomExceptionType.USER_INPUT_ERROR, ErrMsg.REJECT_COMMENT_NEEDED);
            }
            post.setStatus(PostStatus.EDITOR_REJECT.getCode());
        }
        post.setRejectComment(comment);
        post.setRejectCommentTime(new Date());
        postService.updatePost(post);

        return AjaxResponse.success();
    }

    @PutMapping("type=2")
    @PreAuthorize("hasAnyRole('ROLE_CONTRIBUTOR')")
    @ApiOperation(value = "稿件状态为待修改，投稿人修改稿件基本信息")
    @ApiImplicitParam(name = "postQuery", value = "必填：id\n不填：field、fundLevel、writersInfo",
            dataType = "PostQuery")
    public AjaxResponse updatePost5(@Validated({Update2.class}) @RequestBody PostQuery postQuery) {

        // TODO: 检查关键字和作者信息格式

        Post post = postService.getPost(postQuery.getId());

        //        int uid = currentUser.getCurrentUser().getUserId();
        int uid = CONTRIBUTOR_ID;

        // 检查其为稿件投稿人
        if (uid != post.getContributorUid()) {
            throw new CustomException(CustomExceptionType.USER_INPUT_ERROR, ErrMsg.NOT_CONTRIBUTOR);
        }

        // 检查稿件状态
        if (post.getStatus() != PostStatus.TO_BE_REVISED.getCode()) {
            throw new CustomException(CustomExceptionType.USER_INPUT_ERROR, ErrMsg.WRONG_STATUS);
        }

        // 更新稿件基本信息
        BeanUtil.copyProperties(postQuery, post,
                CopyOptions.create().setIgnoreNullValue(true).setIgnoreError(true));
        postService.updatePost(post);

        return AjaxResponse.success();
    }

    @PutMapping("{id}/type=revise")
    @PreAuthorize("hasAnyRole('ROLE_CONTRIBUTOR')")
    @ApiOperation(value = "稿件状态为待修改，投稿人提交待审稿人再审/编辑出版前审核")
    public AjaxResponse updatePost6(@NotNull(message = "id不能为空") @PathVariable Integer id) {

        //        int uid = currentUser.getCurrentUser().getUserId();
        int uid = CONTRIBUTOR_ID;

        // 检查其为稿件投稿人
        Post post = postService.getPost(id);
        if (uid != post.getContributorUid()) {
            throw new CustomException(CustomExceptionType.USER_INPUT_ERROR, ErrMsg.NOT_CONTRIBUTOR);
        }

        // 检查稿件状态
        if (post.getStatus() != PostStatus.TO_BE_REVISED.getCode()) {
            throw new CustomException(CustomExceptionType.USER_INPUT_ERROR, ErrMsg.WRONG_STATUS);
        }

        // 检查稿件体裁
        checkGenre(post);

        // 更新稿件状态
        if (postReviewerService.aggregate(id)) {
            // 如果审稿人无需再审
            post.setStatus(PostStatus.REVIEW_BF_PUB.getCode());
        } else {
            // 提醒审稿人
            post.setStatus(PostStatus.RE_REVIEW.getCode());
        }
        postService.updatePost(post);

        return AjaxResponse.success();
    }

    @PutMapping("type=examBfPub")
    @PreAuthorize("hasAnyRole('ROLE_EDITOR')")
    @ApiOperation(value = "稿件状态为出版前待审，编辑进行审核")
    public AjaxResponse updatePost13(@Validated @RequestBody EditorAuditQuery editorAuditQuery) {
        //        int uid = currentUser.getCurrentUser().getUserId();
        int uid = EDITOR_ID;

        // 获取稿件
        Post post = postService.getPost(editorAuditQuery.getId());

        // 检查操作者为该稿件编辑
        if (uid != post.getEditorUid()) {
            throw new CustomException(CustomExceptionType.USER_INPUT_ERROR, ErrMsg.NOT_EDITOR);
        }

        if (post.getStatus() != PostStatus.REVIEW_BF_PUB.getCode()) {
            throw new CustomException(CustomExceptionType.USER_INPUT_ERROR, ErrMsg.WRONG_STATUS);
        }

        String comment = editorAuditQuery.getComment();
        if (editorAuditQuery.getPass()) {
            // 审核通过进入格式待审核状态
            post.setStatus(PostStatus.FORMAT_TO_BE_REVIEWED.getCode());
        } else {
            // 不通过则回到待修改状态
            if (comment == null || StringUtils.isBlank(comment)) {
                throw new CustomException(CustomExceptionType.USER_INPUT_ERROR, ErrMsg.REJECT_COMMENT_NEEDED);
            }
            post.setStatus(PostStatus.TO_BE_REVISED.getCode());
            // TODO: 通知投稿人
        }

        post.setBfPubComment(comment);
        post.setBfPubCommentTime(new Date());
        postService.updatePost(post);
        return AjaxResponse.success();
    }

    @PutMapping("type=examFormat")
    @PreAuthorize("hasAnyRole('ROLE_EDITOR')")
    @ApiOperation(value = "稿件状态为格式待审核，编辑进行格式审核")
    public AjaxResponse updatePost7(@Validated @RequestBody EditorAuditQuery editorAuditQuery) {
        //        int uid = currentUser.getCurrentUser().getUserId();
        int uid = EDITOR_ID;

        // 获取稿件
        Post post = postService.getPost(editorAuditQuery.getId());

        // 检查操作者为该稿件编辑
        if (uid != post.getEditorUid()) {
            throw new CustomException(CustomExceptionType.USER_INPUT_ERROR, ErrMsg.NOT_EDITOR);
        }

        // 检查稿件状态
        if (post.getStatus() != PostStatus.FORMAT_TO_BE_REVIEWED.getCode()) {
            throw new CustomException(CustomExceptionType.USER_INPUT_ERROR, ErrMsg.WRONG_STATUS);
        }

        String comment = editorAuditQuery.getComment();
        if (editorAuditQuery.getPass()) {
            // 格式审核通过
            post.setStatus(PostStatus.LAYOUT_FEE_TO_BE_DETERMINED.getCode());
        } else {
            // 格式审核不通过
            if (comment == null || StringUtils.isBlank(comment)) {
                throw new CustomException(CustomExceptionType.USER_INPUT_ERROR, ErrMsg.REJECT_COMMENT_NEEDED);
            }
            post.setStatus(PostStatus.FORMAT_TO_BE_MODIFIED.getCode());
            // TODO: 通知投稿人
        }
        post.setFormatComment(comment);
        post.setFormatCommentTime(new Date());
        postService.updatePost(post);
        return AjaxResponse.success();
    }

    @PutMapping("{id}/type=format")
    @PreAuthorize("hasAnyRole('ROLE_CONTRIBUTOR')")
    @ApiOperation(value = "稿件状态为格式待修改，投稿人提交待格式审核")
    public AjaxResponse updatePost8(@NotNull(message = "id不能为空") @PathVariable Integer id) {

        //        int uid = currentUser.getCurrentUser().getUserId();
        int uid = CONTRIBUTOR_ID;

        // 检查其为稿件投稿人
        Post post = postService.getPost(id);
        if (uid != post.getContributorUid()) {
            throw new CustomException(CustomExceptionType.USER_INPUT_ERROR, ErrMsg.NOT_CONTRIBUTOR);
        }

        // 检查稿件状态
        if (post.getStatus() != PostStatus.FORMAT_TO_BE_MODIFIED.getCode()) {
            throw new CustomException(CustomExceptionType.USER_INPUT_ERROR, ErrMsg.WRONG_STATUS);
        }

        // 更新稿件状态
        post.setStatus(PostStatus.FORMAT_TO_BE_REVIEWED.getCode());
        postService.updatePost(post);

        return AjaxResponse.success();
    }

    @PutMapping("type=examPayment")
    @PreAuthorize("hasAnyRole('ROLE_EDITOR')")
    @ApiOperation(value = "稿件状态为缴费证明待审核，编辑进行审核")
    public AjaxResponse updatePost12(@Validated @RequestBody EditorAuditQuery editorAuditQuery) {

        //        int uid = currentUser.getCurrentUser().getUserId();
        int uid = EDITOR_ID;

        // 获取稿件
        Post post = postService.getPost(editorAuditQuery.getId());

        // 检查操作者为该稿件编辑
        if (uid != post.getEditorUid()) {
            throw new CustomException(CustomExceptionType.USER_INPUT_ERROR, ErrMsg.NOT_EDITOR);
        }

        // 检查稿件状态
        if (post.getStatus() != PostStatus.PAYMENT_TO_BE_EXAMINED.getCode()) {
            throw new CustomException(CustomExceptionType.USER_INPUT_ERROR, ErrMsg.WRONG_STATUS);
        }

        String comment = editorAuditQuery.getComment();
        if (editorAuditQuery.getPass()) {
            // 如果缴费证明审核通过
            post.setStatus(PostStatus.SUCCESS.getCode());
        } else {
            // 缴费证明审核不通过
            if (comment == null || StringUtils.isBlank(comment)) {
                throw new CustomException(CustomExceptionType.USER_INPUT_ERROR, ErrMsg.REJECT_COMMENT_NEEDED);
            }
            post.setStatus(PostStatus.CERTIFICATE_TO_BE_UPLOADED.getCode());
        }
        post.setCertificateComment(comment);
        post.setCertificateCommentTime(new Date());
        postService.updatePost(post);
        return AjaxResponse.success();
    }

    @PutMapping("type=fee")
    @PreAuthorize("hasAnyRole('ROLE_EDITOR')")
    @ApiOperation(value = "稿件状态为待确定版面费，编辑确定版面费")
    public AjaxResponse updatePost9(@Validated @RequestBody PostLayOutFeeQuery postLayOutFeeQuery) {
        Post post = postService.getPost(postLayOutFeeQuery.getId());

        //        int uid = currentUser.getCurrentUser().getUserId();
        int uid = EDITOR_ID;

        // 检查操作用户为编辑
        if (uid != post.getEditorUid()) {
            throw new CustomException(CustomExceptionType.USER_INPUT_ERROR, ErrMsg.NOT_EDITOR);
        }

        // 检查状态为待确定版面费
        if (post.getStatus() != PostStatus.LAYOUT_FEE_TO_BE_DETERMINED.getCode()) {
            throw new CustomException(CustomExceptionType.USER_INPUT_ERROR, ErrMsg.WRONG_STATUS);
        }

        // 设置版面费和状态
        post.setFee(Double.valueOf(postLayOutFeeQuery.getFee()));
        post.setStatus(PostStatus.CERTIFICATE_TO_BE_UPLOADED.getCode());

        // TODO: 通知缴费

        postService.updatePost(post);

        return AjaxResponse.success();
    }

    @PutMapping("type=receipt")
    @PreAuthorize("hasAnyRole('ROLE_CONTRIBUTOR')")
    @ApiOperation(value = "稿件状态为缴费证明待上传，投稿人填写收据信息")
    public AjaxResponse updatePost10(@Validated @RequestBody PostReceiptQuery postReceiptQuery) {
        Post post = postService.getPost(postReceiptQuery.getId());

        //        int uid = currentUser.getCurrentUser().getUserId();
        int uid = CONTRIBUTOR_ID;

        // 检查操作用户为投稿人
        if (uid != post.getContributorUid()) {
            throw new CustomException(CustomExceptionType.USER_INPUT_ERROR, ErrMsg.NOT_CONTRIBUTOR);
        }

        // 检查状态
        if (post.getStatus() != PostStatus.CERTIFICATE_TO_BE_UPLOADED.getCode()) {
            throw new CustomException(CustomExceptionType.USER_INPUT_ERROR, ErrMsg.WRONG_STATUS);
        }

        // 更新收据信息
        post.setInvoiceNeeded(postReceiptQuery.isInvoiceNeeded() ? MyBoolean.TRUE.getCode() : MyBoolean.FALSE.getCode());
        post.setInvoiceTitle(postReceiptQuery.getInvoiceTitle());
        post.setTaxpayerId(postReceiptQuery.getTaxpayerId());
        post.setReceiptReceiver(postReceiptQuery.getReceiptReceiver());
        post.setReceiptAddress(postReceiptQuery.getReceiptAddress());
        post.setReceiverPhone(postReceiptQuery.getReceiverPhone());
        postService.updatePost(post);

        return AjaxResponse.success();
    }

    @PutMapping("{id}/type=payment")
    @PreAuthorize("hasAnyRole('ROLE_CONTRIBUTOR')")
    @ApiOperation(value = "稿件状态为缴费证明待上传，投稿人提交待审核")
    public AjaxResponse updatePost11(@NotNull(message = "id不能为空") @PathVariable Integer id) {
        //        int uid = currentUser.getCurrentUser().getUserId();
        int uid = CONTRIBUTOR_ID;

        // 检查其为稿件投稿人
        Post post = postService.getPost(id);
        if (uid != post.getContributorUid()) {
            throw new CustomException(CustomExceptionType.USER_INPUT_ERROR, ErrMsg.NOT_CONTRIBUTOR);
        }

        // 检查稿件状态
        if (post.getStatus() != PostStatus.CERTIFICATE_TO_BE_UPLOADED.getCode()) {
            throw new CustomException(CustomExceptionType.USER_INPUT_ERROR, ErrMsg.WRONG_STATUS);
        }

        // 检查收据信息、缴费证明是否完整
        checkPaymentInfo(post);

        // 更新稿件状态
        post.setStatus(PostStatus.PAYMENT_TO_BE_EXAMINED.getCode());
        postService.updatePost(post);

        return AjaxResponse.success();
    }

    @ApiOperation(value = "管理员根据id删除期刊记录")
    @DeleteMapping("/{id}")
    @PreAuthorize("hasAnyRole('ROLE_ADMIN')")
    public AjaxResponse deletePost(@NotNull(message = "id不能为空") @PathVariable Integer id) {
        Post post = postService.getPost(id);
        if (post.getPostPath() != null) {
            FileUtil.deleteFile(post.getPostPath());
        }
        if (post.getEthicsApprovalPath() != null) {
            FileUtil.deleteFile(post.getEthicsApprovalPath());
        }
        if (post.getLetterPath() != null) {
            FileUtil.deleteFile(post.getLetterPath());
        }
        if (post.getFundApprovalPath() != null) {
            FileUtil.deleteFile(post.getFundApprovalPath());
        }
        if (post.getCertificatePath() != null) {
            FileUtil.deleteFile(post.getCertificatePath());
        }
        postService.deletePost(id);
        return AjaxResponse.success();
    }

    @GetMapping("role=ctr")
    @PreAuthorize("hasAnyRole('ROLE_CONTRIBUTOR')")
    @ApiOperation(value = "投稿人获取自己的稿件列表")
    @ApiImplicitParams({
            @ApiImplicitParam(name = "number", value = "分页页号", required = true, dataType = "int"),
            @ApiImplicitParam(name = "size", value = "分页大小", required = true, dataType = "int"),
            @ApiImplicitParam(name = "ascend", value = "是否升序，默认为true", dataType = "boolean"),
            @ApiImplicitParam(name = "statuses", value = "要筛选的状态，默认为所有状态", allowMultiple = true,dataType = "int"),
            @ApiImplicitParam(name = "start", value = "开始提交日期，start与end成对使用，默认为所有时间", dataType = "date-time"),
            @ApiImplicitParam(name = "end", value = "结束提交日期，start与end成对使用，默认为所有时间", dataType = "date-time")
    })
    public AjaxResponse getAll1(@NotNull(message = "number不能为空") @RequestParam("number") Integer number,
                                @NotNull(message = "size不能为空") @RequestParam("size") Integer size,
                                @RequestParam(value = "ascend", required = false) Boolean ascend,
                                @RequestParam(value = "statuses", required = false) List<Integer> statuses,
                                @RequestParam(value = "start", required = false) Date start,
                                @RequestParam(value = "end", required = false) Date end) {

        checkStartAndEndTime(start, end);

        //        int uid = currentUser.getCurrentUser().getUserId();
        int uid = CONTRIBUTOR_ID;

        if (ascend == null) {
            ascend = true;
        }
        Page<Post> page;

        if (statuses == null || statuses.isEmpty()) {
            if (start == null) {
                page = postService.getAllByCtrUid(uid, number, size, ascend);
            } else {
                page = postService.getAllByCtrUidAndSubmitTime(uid, start, end, number, size, ascend);
            }
        } else {
            // 检查statuses列表
            for(Integer code: statuses){
                PostStatus postStatus = PostStatus.getItem(code);
                if (postStatus == null) {
                    throw new CustomException(CustomExceptionType.USER_INPUT_ERROR, ErrMsg.UNSUPPORTED_STATUS);
                }
            }

            if (start == null) {
                page = postService.getAllByCtrUidAndStatus(uid, statuses, number, size, ascend);
            } else {
                page = postService.getAllByCtrUidAndStatusAndSubmitTime(uid, statuses, start, end, number, size, ascend);
            }
        }

        return AjaxResponse.success(DozerUtil.mapPageT(page, Post4CtrSimpleVO.class));
    }

    @GetMapping("role=ed")
    @PreAuthorize("hasAnyRole('ROLE_EDITOR')")
    @ApiOperation(value = "编辑获取负责编辑的稿件列表")
    @ApiImplicitParams({
            @ApiImplicitParam(name = "number", value = "分页页号", required = true, dataType = "int"),
            @ApiImplicitParam(name = "size", value = "分页大小", required = true, dataType = "int"),
            @ApiImplicitParam(name = "ascend", value = "是否升序，默认为true", dataType = "boolean"),
            @ApiImplicitParam(name = "statuses", value = "要筛选的状态，默认为所有状态", allowMultiple = true,dataType = "int"),
            @ApiImplicitParam(name = "start", value = "开始提交日期，start与end成对使用，默认为所有时间", dataType = "date-time"),
            @ApiImplicitParam(name = "end", value = "结束提交日期，start与end成对使用，默认为所有时间", dataType = "date-time")
    })
    public AjaxResponse getAll2(@NotNull(message = "number不能为空") @RequestParam("number") Integer number,
                                @NotNull(message = "size不能为空") @RequestParam("size") Integer size,
                                @RequestParam(value = "ascend", required = false) Boolean ascend,
                                @RequestParam(value = "statuses", required = false) List<Integer> statuses,
                                @RequestParam(value = "start", required = false) Date start,
                                @RequestParam(value = "end", required = false) Date end) {

        checkStartAndEndTime(start, end);

        // int uid = currentUser.getCurrentUser().getUserId();
        int uid = EDITOR_ID;

        if (ascend == null) {
            ascend = true;
        }
        Page<Post> page;

        if (statuses == null || statuses.isEmpty()) {
            if (start == null) {
                page = postService.getAllByEdUid(uid, number, size, ascend);
            } else {
                page = postService.getAllByEdUidAndSubmitTime(uid, start, end, number, size, ascend);
            }
        } else {
            // 检查statuses列表
            for(Integer code: statuses){
                PostStatus postStatus = PostStatus.getItem(code);
                if (postStatus == null) {
                    throw new CustomException(CustomExceptionType.USER_INPUT_ERROR, ErrMsg.UNSUPPORTED_STATUS);
                }
            }
            if (start == null) {
                page = postService.getAllByEdUidAndStatus(uid, statuses, number, size, ascend);
            } else {
                page = postService.getAllByEdUidAndStatusAndSubmitTime(uid, statuses, start, end, number, size, ascend);
            }
        }

        List<Post> pageContent = page.getContent();
        PageDataT<Post4EdSimpleVO> retData = DozerUtil.mapPageT(page, Post4EdSimpleVO.class);

        for (int i = 0; i < page.getNumberOfElements(); i++) {
            Post4EdSimpleVO item = retData.getList().get(i);
            item.setContributor(getName(pageContent.get(i).getContributorUid()));
        }

        return AjaxResponse.success(retData);
    }

    // TODO: 支持更多类型的通知

    private void notifyContributor(Post post) {
        // 给投稿人发送邮件
        Optional<User> userOptional = userService.findUserByUid(post.getContributorUid());
        if (!userOptional.isPresent()) {
            throw new CustomException(CustomExceptionType.USER_INPUT_ERROR, ErrMsg.CONTRIBUTOR_NOT_FOUND);
        }
        User ctr = userOptional.get();
        mailService.sendHtmlMailForContributor(ctr.getEmail(), post.getTitle(), ctr.getUserName());
    }

    private void notifyReviewer(Post post) {
        // 给审稿人发送邮件
        List<PostReviewer> postReviewers = postReviewerService.getAllByPidAndFlag(post.getId(), true);
        for (PostReviewer item : postReviewers) {
            Optional<User> userOptional = userService.findUserByUid(item.getReviewerUid());
            if (!userOptional.isPresent()) {
                throw new CustomException(CustomExceptionType.USER_INPUT_ERROR, ErrMsg.CONTRIBUTOR_NOT_FOUND);
            }
            User rev = userOptional.get();
            mailService.sendHtmlMailForReviewer(rev.getEmail(), post.getTitle(), rev.getUserName());
        }
    }

    private void checkStartAndEndTime(Date start, Date end) {
        if (start == null && end != null || end == null && start != null) {
            throw new CustomException(CustomExceptionType.USER_INPUT_ERROR, ErrMsg.WRONG_TIME);
        }
        if (start != null && start.after(end)) {
            throw new CustomException(CustomExceptionType.USER_INPUT_ERROR, ErrMsg.WRONG_TIME);
        }
    }

    private void checkGenre(Post post) {

        Integer genre = post.getGenre();
        if (genre == null || Genre.getItem(post.getGenre()) == null) {
            throw new CustomException(CustomExceptionType.USER_INPUT_ERROR, ErrMsg.GENRE_NEEDED);
        }

        // 检查标题、关键字和摘要填写情况
        String abstractZh = post.getAbstractZh();
        String abstractEn = post.getAbstractEn();
        String keywordsZh = post.getKeywordsZh();
        String keywordsEn = post.getKeywordsEn();
        String titleEn = post.getTitleEn();
        boolean hasZhKw = keywordsZh != null && !StringUtils.isBlank(keywordsZh);
        boolean hasEnKw = keywordsEn != null && !StringUtils.isBlank(keywordsEn);
        boolean hasZhAbstract = abstractZh != null && !StringUtils.isBlank(abstractZh);
        boolean hasEnAbstract = abstractEn != null && !StringUtils.isBlank(abstractEn);
        boolean hasTitleEn = titleEn != null && !StringUtils.isBlank(titleEn);

        if (genre == WORKS.getCode()) {
            // TODO: 假设专著也需要英文标题
            if (!(hasTitleEn && hasZhKw && hasEnKw && hasZhAbstract && hasEnAbstract)) {
                throw new CustomException(CustomExceptionType.USER_INPUT_ERROR, ErrMsg.INCOMPLETE_INFO_REQUIRED_FOR_THE_GENRE);
            }
        } else if (genre == OVERVIEW.getCode()) {
            if (!(hasTitleEn && hasZhKw && hasZhAbstract)) {
                throw new CustomException(CustomExceptionType.USER_INPUT_ERROR, ErrMsg.INCOMPLETE_INFO_REQUIRED_FOR_THE_GENRE);
            }
        } else if (genre == PAPER.getCode()) {
            if (!(hasTitleEn && hasZhKw && hasZhAbstract)) {
                throw new CustomException(CustomExceptionType.USER_INPUT_ERROR, ErrMsg.INCOMPLETE_INFO_REQUIRED_FOR_THE_GENRE);
            }
        } else if (Genre.getItem(genre) == null) {
            throw new CustomException(CustomExceptionType.USER_INPUT_ERROR, ErrMsg.UNSUPPORTED_GENRE);
        }
    }

    private void checkPostCanSubmit(Post post) {
        // 检查标题信息
        String title = post.getTitle();
        if (title == null || StringUtils.isBlank(title)) {
            throw new CustomException(CustomExceptionType.USER_INPUT_ERROR, ErrMsg.TITLE_NEEDED);
        }

        // 检查稿件领域信息
        if (post.getField() == null || Field.getItem(post.getField()) == null) {
            throw new CustomException(CustomExceptionType.USER_INPUT_ERROR, ErrMsg.FIELD_NEEDED);
        }

        // 检查作者信息
        String writersInfo = post.getWritersInfo();
        if (writersInfo == null || StringUtils.isBlank(writersInfo)) {
            throw new CustomException(CustomExceptionType.USER_INPUT_ERROR, ErrMsg.WRITERS_INFO_NEEDED);
        }

        // 检查基金级别
        Integer fundLevel = post.getFundLevel();
        if (fundLevel == null || FundLevel.getItem(fundLevel) == null) {
            throw new CustomException(CustomExceptionType.USER_INPUT_ERROR, ErrMsg.FUND_LEVEL_NEEDED);
        }

        // 检查体裁的中英文关键字和中英摘要等情况
        checkGenre(post);

        // 检查文件上传情况
        String fundApprovalPath = post.getFundApprovalPath();
        if (post.getPostPath() == null || post.getLetterPath() == null || post.getEthicsApprovalPath() == null) {
            throw new CustomException(CustomExceptionType.USER_INPUT_ERROR, ErrMsg.INCOMPLETE_POST_FILES);
        }
        System.out.println("aaaa"+fundLevel+FundLevel.NO.getCode());
        if (fundLevel != FundLevel.NO.getCode() && fundApprovalPath == null) {
            throw new CustomException(CustomExceptionType.USER_INPUT_ERROR, ErrMsg.INCOMPLETE_POST_FILES);
        }
    }

    private void checkPaymentInfo(Post post) {
        Integer invoiceNeeded = post.getInvoiceNeeded();
        String invoiceTitle = post.getInvoiceTitle();
        String taxpayerId = post.getTaxpayerId();
        String address = post.getReceiptAddress();
        String receiver = post.getReceiptReceiver();
        String certificate = post.getCertificatePath();

        // 检查缴费证明是否上传
        if (certificate == null) {
            throw new CustomException(CustomExceptionType.USER_INPUT_ERROR, ErrMsg.PAYMENT_FILE_NOT_UPLOADED);
        }

        if (invoiceNeeded == MyBoolean.DEFAULT.getCode()) {
            // 投稿人没填是否需要发票
            throw new CustomException(CustomExceptionType.USER_INPUT_ERROR, ErrMsg.INVOICE_REQUIRED_OR_NOT_NEEDED);
        } else if (invoiceNeeded == MyBoolean.TRUE.getCode()) {
            // 如果需要发票，检查相关信息是否填写完整
            if (taxpayerId == null || StringUtils.isBlank(taxpayerId) ||
                    address == null || StringUtils.isBlank(address) ||
                    receiver == null || StringUtils.isBlank(receiver) ||
                    invoiceTitle == null || StringUtils.isBlank(invoiceTitle)) {
                throw new CustomException(CustomExceptionType.USER_INPUT_ERROR, ErrMsg.INCOMPLETE_INVOICE_INFO);
            }
        }
    }

    private String getName(int uid) {
        final int WRITTEN_OFF = -1;
        String ret;
        Optional<User> userOptional = userService.findUserByUid(uid);
        if (userOptional.isPresent()) {
            User u = userOptional.get();
            ret = u.getActive() == WRITTEN_OFF ? Names.WRITTEN_OFF_USER : u.getUserName();
        } else {
            ret = Names.DELETED_USER;
        }
        return ret;
    }
}
