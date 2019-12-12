package com.samsph.pjcm.web.controller;

import cn.hutool.core.bean.BeanUtil;
import cn.hutool.core.bean.copier.CopyOptions;
import com.samsph.pjcm.config.DevUserId;
import com.samsph.pjcm.config.auth.CurrentUser;
import com.samsph.pjcm.config.constant.*;
import com.samsph.pjcm.config.exception.AjaxResponse;
import com.samsph.pjcm.config.exception.CustomException;
import com.samsph.pjcm.config.exception.CustomExceptionType;
import com.samsph.pjcm.config.utils.DozerUtil;
import com.samsph.pjcm.model.Journal;
import com.samsph.pjcm.model.Post;
import com.samsph.pjcm.model.PostReviewer;
import com.samsph.pjcm.query.*;
import com.samsph.pjcm.service.EditorFieldService;
import com.samsph.pjcm.service.JournalService;
import com.samsph.pjcm.service.PostReviewerService;
import com.samsph.pjcm.service.PostService;
import com.samsph.pjcm.vo.*;
import io.swagger.annotations.Api;
import io.swagger.annotations.ApiImplicitParam;
import io.swagger.annotations.ApiOperation;
import org.dozer.Mapper;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.data.domain.Page;
import org.springframework.security.access.prepost.PreAuthorize;
import org.springframework.validation.annotation.Validated;
import org.springframework.web.bind.annotation.*;

import javax.annotation.Resource;
import javax.validation.constraints.NotNull;

import java.util.Date;

import static com.samsph.pjcm.config.DevUserId.*;
import static com.samsph.pjcm.config.constant.Genre.*;


/**
 * @author hujiahao
 */

@Validated
@RestController
@RequestMapping("/api/v1/posts")
@Api(tags = "2. 投稿管理")
public class PostController {
    @Resource
    private Mapper dozerMapper;

    @Resource
    PostService postService;

    @Resource
    JournalService journalService;

    @Resource
    PostReviewerService postReviewerService;

    @Autowired
    private CurrentUser currentUser;

    @Autowired
    private EditorFieldService editorFieldService;

    @PostMapping()
    @PreAuthorize("hasAnyRole('ROLE_CONTRIBUTOR')")
    @ApiOperation(value = "投稿人创建一次投稿")
    @ApiImplicitParam(name = "postQuery",
            value = "必填：field、title、genre、fundLevel、writersInfo\n" +
                    "不填：id\n" +
                    "TODO：服务器端对中英文关键词和作者信息做格式校验",
            dataType = "PostQuery")
    public AjaxResponse savePost(@Validated({Add.class}) @RequestBody PostQuery postQuery) {

//        int uid = currentUser.getCurrentUser().getUserId();
        int uid = CONTRIBUTOR_ID;

        Post post = postService.savePost(postQuery, uid);

        return AjaxResponse.success(dozerMapper.map(post, Post4CtrSimpleVO.class));
    }

    @PutMapping("type=1")
    @PreAuthorize("hasAnyRole('ROLE_CONTRIBUTOR')")
    @ApiOperation(value = "稿件状态为待提交，投稿人修改投稿基本信息")
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

        // 更新稿件基本信息
        BeanUtil.copyProperties(postQuery, post,
                CopyOptions.create().setIgnoreNullValue(true).setIgnoreError(true));
        postService.updatePost(post);

        return AjaxResponse.success();
    }

    @PutMapping("type=2")
    @PreAuthorize("hasAnyRole('ROLE_CONTRIBUTOR')")
    @ApiOperation(value = "稿件状态为待提交，投稿人提交以待初审")
    public AjaxResponse updatePost2(@NotNull(message = "id不能为空") @RequestBody Integer id) {
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

        // 检查投稿信息填写完整，能提交初审
        checkPostCanSubmit(post);

        int size = editorFieldService.findByFieldId(post.getField()).size();
        if(size == 0){
            throw new CustomException(CustomExceptionType.SYSTEM_ERROR,"该领域下无编辑");
        }
        int num = (int)(1+Math.random()*(size-1+1));
        int editorId = editorFieldService.findByFieldId(post.getField()).get(num-1).getEditorUid();
        post.setEditorUid(editorId);
        post.setStatus(PostStatus.PENDING_FIRST_EXAM.getCode());

        postService.updatePost(post);

        return AjaxResponse.success();
    }

    @PutMapping("type=3")
    @PreAuthorize("hasAnyRole('ROLE_EDITOR')")
    @ApiOperation(value = "稿件状态为待初审，编辑进行初审")
    public AjaxResponse updatePost3(@RequestBody EditorAuditQuery editorAuditQuery) {
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

        if (editorAuditQuery.getPass()) {
            // 如果初审通过
            post.setSubmitTime(new Date());
            post.setStatus(PostStatus.REVIEWER_TO_BE_SELECTED.getCode());
        } else {
            // 初审不通过
            String comment = editorAuditQuery.getComment();
            if (comment == null || comment.isBlank()) {
                throw new CustomException(CustomExceptionType.USER_INPUT_ERROR, ErrMsg.REJECT_COMMENT_NEEDED);
            }
            post.setFirstExamComment(comment);
            post.setFirstExamCommentTime(new Date());
            post.setStatus(PostStatus.FIRST_EXAM_REJECTED.getCode());
        }
        postService.updatePost(post);

        return AjaxResponse.success();
    }

    @PutMapping("type=4")
    @PreAuthorize("hasAnyRole('ROLE_EDITOR')")
    @ApiOperation(value = "稿件状态为待退回，编辑选择退改或退稿")
    public AjaxResponse updatePost4(@RequestBody EditorAuditQuery editorAuditQuery) {
        //        int uid = currentUser.getCurrentUser().getUserId();
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

        if (editorAuditQuery.getPass()) {
            // 编辑决定退回修改
            post.setStatus(PostStatus.TO_BE_REVISED.getCode());
            post.setCount(post.getCount() + 1);
        } else {
            // 编辑退稿
            String comment = editorAuditQuery.getComment();
            if (comment == null || comment.isBlank()) {
                throw new CustomException(CustomExceptionType.USER_INPUT_ERROR, ErrMsg.REJECT_COMMENT_NEEDED);
            }
            // 编辑退稿
            post.setRejectComment(comment);
            post.setRejectCommentTime(new Date());
            post.setStatus(PostStatus.EDITOR_REJECT.getCode());
        }
        postService.updatePost(post);

        return AjaxResponse.success();
    }

    @PutMapping("type=5")
    @PreAuthorize("hasAnyRole('ROLE_CONTRIBUTOR')")
    @ApiOperation(value = "稿件状态为待修改，投稿人修改稿件基本信息")
    public AjaxResponse updatePost5(@Validated({Update2.class}) @RequestBody PostQuery postQuery) {

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

    @PutMapping("type=6")
    @PreAuthorize("hasAnyRole('ROLE_CONTRIBUTOR')")
    @ApiOperation(value = "稿件状态为待修改，投稿人提交待再审")
    public AjaxResponse updatePost6(@NotNull(message = "id不能为空") @RequestBody Integer id) {

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

        // 更新稿件状态
        post.setStatus(PostStatus.RE_REVIEW.getCode());
        postService.updatePost(post);

        return AjaxResponse.success();
    }

    @PutMapping("type=7")
    @PreAuthorize("hasAnyRole('ROLE_EDITOR')")
    @ApiOperation(value = "稿件状态为格式待审核，编辑进行格式审核")
    public AjaxResponse updatePost7(@RequestBody EditorAuditQuery editorAuditQuery) {
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

        if (editorAuditQuery.getPass()) {
            // 如果格式审核通过
            post.setStatus(PostStatus.LAYOUT_FEE_TO_BE_DETERMINED.getCode());
        } else {
            // 格式审核不通过
            String comment = editorAuditQuery.getComment();
            if (comment == null || comment.isBlank()) {
                throw new CustomException(CustomExceptionType.USER_INPUT_ERROR, ErrMsg.REJECT_COMMENT_NEEDED);
            }
            post.setFormatComment(comment);
            post.setFormatCommentTime(new Date());
            post.setStatus(PostStatus.FORMAT_TO_BE_MODIFIED.getCode());
        }
        postService.updatePost(post);
        return AjaxResponse.success();
    }

    @PutMapping("type=8")
    @PreAuthorize("hasAnyRole('ROLE_CONTRIBUTOR')")
    @ApiOperation(value = "稿件状态为格式待修改，投稿人提交待格式审核")
    public AjaxResponse updatePost8(@NotNull(message = "id不能为空") @RequestBody Integer id) {

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

    @PutMapping("type=9")
    @PreAuthorize("hasAnyRole('ROLE_EDITOR')")
    @ApiOperation(value = "稿件状态为待确定版面费，编辑确定版面费")
    public AjaxResponse updatePost9(@RequestBody PostLayOutFeeQuery postLayOutFeeQuery) {
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

        // 设置版面费
        post.setFee(postLayOutFeeQuery.getFee());
        // 更新状态
        post.setStatus(PostStatus.CERTIFICATE_TO_BE_UPLOADED.getCode());
        postService.updatePost(post);

        return AjaxResponse.success();
    }

    @PutMapping("type=10")
    @PreAuthorize("hasAnyRole('ROLE_CONTRIBUTOR')")
    @ApiOperation(value = "稿件状态为缴费证明待上传，投稿人填写收据信息")
    public AjaxResponse updatePost10(@RequestBody PostReceiptQuery postReceiptQuery) {
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
        BeanUtil.copyProperties(postReceiptQuery, post);
        postService.updatePost(post);

        return AjaxResponse.success();
    }

    @PutMapping("type=11")
    @PreAuthorize("hasAnyRole('ROLE_CONTRIBUTOR')")
    @ApiOperation(value = "稿件状态为缴费证明待上传，投稿人确认提交")
    public AjaxResponse updatePost11(@NotNull(message = "id不能为空") @RequestBody Integer id) {
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

    @PutMapping("type=12")
    @PreAuthorize("hasAnyRole('ROLE_EDITOR')")
    @ApiOperation(value = "稿件状态为缴费证明待审核，编辑进行审核")
    public AjaxResponse updatePost12(@RequestBody EditorAuditQuery editorAuditQuery) {

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

        if (editorAuditQuery.getPass()) {
            // 如果缴费证明审核通过
            post.setStatus(PostStatus.SUCCESS.getCode());
        } else {
            // 缴费证明审核不通过
            String comment = editorAuditQuery.getComment();
            if (comment == null || comment.isBlank()) {
                throw new CustomException(CustomExceptionType.USER_INPUT_ERROR, ErrMsg.REJECT_COMMENT_NEEDED);
            }
            post.setCertificateComment(comment);
            post.setCertificateCommentTime(new Date());
            post.setStatus(PostStatus.CERTIFICATE_TO_BE_UPLOADED.getCode());
        }
        postService.updatePost(post);
        return AjaxResponse.success();
    }

    @PutMapping("type=13")
    @PreAuthorize("hasAnyRole('ROLE_ADMIN')")
    @ApiOperation(value = "稿件状态为投稿成功，管理员将稿件加入到某一期期刊")
    public AjaxResponse updatePost13(@RequestBody PostJournalQuery postJournalQuery) {

        Post post = postService.getPost(postJournalQuery.getId());

        // 检查状态
        if (post.getStatus() != PostStatus.SUCCESS.getCode()) {
            throw new CustomException(CustomExceptionType.USER_INPUT_ERROR, ErrMsg.WRONG_STATUS);
        }

        Integer jid = postJournalQuery.getJid();

        if (jid == null) {
            if (post.getJid() != null) {
                // 取消投稿的原期刊信息
                post.setJid(null);
                Journal journal = journalService.getJournal(post.getJid());
                journal.setTotal(journal.getTotal() - 1);
                journalService.updateJournal(journal);
            }
        } else {
            // 查找要设置的期刊是否存在
            Journal journal = journalService.getJournal(jid);

            if (post.getJid() == null) {
                // 新增投稿的期刊信息
                post.setJid(jid);
                journal.setTotal(journal.getTotal() + 1);
                journalService.updateJournal(journal);
                post.setJid(jid);
                postService.updatePost(post);
            } else if (!jid.equals(post.getJid())) {
                // 覆盖投稿的期刊信息
                journal.setTotal(journal.getTotal() + 1);
                journalService.updateJournal(journal);

                Journal oldJournal = journalService.getJournal(post.getJid());
                oldJournal.setTotal(oldJournal.getTotal() - 1);
                journalService.updateJournal(oldJournal);

                post.setJid(jid);
                postService.updatePost(post);
            }
        }
        return AjaxResponse.success();
    }


    @GetMapping("/{id}")
    @ApiOperation(value = "游客根据id获取投稿成功的稿件")
    public AjaxResponse getPost(@NotNull @PathVariable Integer id) {
        Post post = postService.getPost(id);

        if (post.getStatus() != PostStatus.SUCCESS.getCode()) {
            throw new CustomException(CustomExceptionType.USER_INPUT_ERROR, ErrMsg.WRONG_STATUS);
        }

        return AjaxResponse.success(dozerMapper.map(post, Post4TurVO.class));
    }

    @GetMapping()
    @ApiOperation(value = "游客根据期刊获取所包含投稿列表")
    public AjaxResponse getAll(@NotNull(message = "jid不能为空") @RequestParam("jid") Integer jid,
                        @NotNull(message = "number不能为空") @RequestParam("number") Integer number,
                        @NotNull(message = "size不能为空") @RequestParam("size") Integer size,
                        @RequestParam(value = "ascend", required = false) Boolean ascend) {
        if (ascend == null) {
            ascend = true;
        }

        Page<Post> page = postService.getAllByJid(jid, number, size, ascend);

        return AjaxResponse.success(DozerUtil.mapPage(page, Post4TurSimpleVO.class));
    }

    @GetMapping("/{id}/type=1")
    @PreAuthorize("hasAnyRole('ROLE_CONTRIBUTOR')")
    @ApiOperation(value = "投稿人根据id获取自己的投稿")
    public AjaxResponse getPost1(@NotNull @PathVariable Integer id) {
        Post post = postService.getPost(id);

        //        int uid = currentUser.getCurrentUser().getUserId();
        int uid = CONTRIBUTOR_ID;

        // 检查其为稿件投稿人
        if (uid != post.getContributorUid()) {
            throw new CustomException(CustomExceptionType.USER_INPUT_ERROR, ErrMsg.NOT_CONTRIBUTOR);
        }

        return AjaxResponse.success(dozerMapper.map(post, Post4CtrVO.class));
    }

    @GetMapping("type=1")
    @PreAuthorize("hasAnyRole('ROLE_CONTRIBUTOR')")
    @ApiOperation(value = "投稿人获取自己的投稿列表")
    public AjaxResponse getAll1(@NotNull(message = "number不能为空") @RequestParam("number") Integer number,
                         @NotNull(message = "size不能为空") @RequestParam("size") Integer size,
                         @RequestParam(value = "ascend", required = false) Boolean ascend,
                         @RequestParam(value = "status", required = false) PostStatus status) {

        //        int uid = currentUser.getCurrentUser().getUserId();
        int uid = CONTRIBUTOR_ID;

        if (ascend == null) {
            ascend = true;
        }
        Page<Post> page;

        if (status == null) {
            page = postService.getAllByCtr(uid, number, size, ascend);
        } else {
            page = postService.getAllByCtrAndStatus(uid, status, number, size, ascend);
        }

        return AjaxResponse.success(DozerUtil.mapPage(page, Post4CtrSimpleVO.class));
    }

    @GetMapping("/{id}/type=2")
    @PreAuthorize("hasAnyRole('ROLE_EDITOR')")
    @ApiOperation(value = "编辑根据id获取负责编辑的稿件")
    public AjaxResponse getPost2(@NotNull @PathVariable Integer id) {
        Post post = postService.getPost(id);

        //        int uid = currentUser.getCurrentUser().getUserId();
        int uid = EDITOR_ID;

        // 检查其为稿件编辑
        if (uid != post.getEditorUid()) {
            throw new CustomException(CustomExceptionType.USER_INPUT_ERROR, ErrMsg.NOT_EDITOR);
        }

        return AjaxResponse.success(dozerMapper.map(post, Post4EdVO.class));
    }

    @GetMapping("type=2")
    @PreAuthorize("hasAnyRole('ROLE_EDITOR')")
    @ApiOperation(value = "编辑获取负责编辑的稿件列表")
    public AjaxResponse getAll2(@NotNull(message = "number不能为空") @RequestParam("number") Integer number,
                         @NotNull(message = "size不能为空") @RequestParam("size") Integer size,
                         @RequestParam(value = "ascend", required = false) Boolean ascend,
                         @RequestParam(value = "status", required = false) PostStatus status) {
        //        int uid = currentUser.getCurrentUser().getUserId();
        int uid = EDITOR_ID;

        if (ascend == null) {
            ascend = true;
        }
        Page<Post> page;

        if (status == null) {
            page = postService.getAllByEd(uid, number, size, ascend);
        } else {
            page = postService.getAllByEdAndStatus(uid, status, number, size, ascend);
        }

        return AjaxResponse.success(DozerUtil.mapPage(page, Post4EdSimpleVO.class));
    }

    @GetMapping("/{id}/type=3")
    @PreAuthorize("hasAnyRole('ROLE_REVIEWER')")
    @ApiOperation(value = "审稿人根据id获取待答复的稿件")
    public AjaxResponse getPost3(@NotNull @PathVariable Integer id) {

        //        int uid = currentUser.getCurrentUser().getUserId();
        int uid = REVIEWER_ID;

        // 检查该审稿人未答复
        PostReviewer postReviewer = postReviewerService.getPostReviewer(id, uid);
        if (postReviewer.getAccepted() != MyBoolean.DEFAULT.getCode()) {
            throw new CustomException(CustomExceptionType.USER_INPUT_ERROR, ErrMsg.REVIEWER_HAS_REPLIED);
        }

        Post post = postService.getPost(id);

        return AjaxResponse.success(dozerMapper.map(post, Post4RevVO.class));
    }

    @GetMapping("type=3")
    @PreAuthorize("hasAnyRole('ROLE_REVIEWER')")
    @ApiOperation(value = "审稿人获取待答复的稿件列表")
    public AjaxResponse getAll3(@NotNull(message = "number不能为空") @RequestParam("number") Integer number,
                         @NotNull(message = "size不能为空") @RequestParam("size") Integer size,
                         @RequestParam(value = "ascend", required = false) Boolean ascend) {
        //        int uid = currentUser.getCurrentUser().getUserId();
        int uid = REVIEWER_ID;

        if (ascend == null) {
            ascend = true;
        }
        Page<Post> page = postService.getAllByRevUnanswer(uid, number, size, ascend);

        return AjaxResponse.success(DozerUtil.mapPage(page, Post4RevSimpleVO.class));
    }

    @GetMapping("/{id}/type=4")
    @PreAuthorize("hasAnyRole('ROLE_REVIEWER')")
    @ApiOperation(value = "审稿人根据id获取负责审稿的稿件")
    public AjaxResponse getPost4(@NotNull @PathVariable Integer id) {
        //        int uid = currentUser.getCurrentUser().getUserId();
        int uid = REVIEWER_ID;

        // 检查稿件状态
        Post post = postService.getPost(id);
        if (post.getStatus() != PostStatus.FIRST_REVIEW.getCode() || post.getStatus() != PostStatus.RE_REVIEW.getCode()) {
            throw new CustomException(CustomExceptionType.USER_INPUT_ERROR, ErrMsg.WRONG_STATUS);
        }

        // 检查该审稿人已接收审稿
        PostReviewer postReviewer = postReviewerService.getPostReviewer(id, uid);
        if (postReviewer.getAccepted() != MyBoolean.TRUE.getCode()) {
            throw new CustomException(CustomExceptionType.USER_INPUT_ERROR, ErrMsg.REVIEWER_HAS_REPLIED);
        }

        return AjaxResponse.success(dozerMapper.map(post, Post4RevVO.class));
    }

    @GetMapping("type=4")
    @PreAuthorize("hasAnyRole('ROLE_REVIEWER')")
    @ApiOperation(value = "审稿人获取已接受且正处于审稿中的稿件列表")
    public AjaxResponse getAll4(@NotNull(message = "number不能为空") @RequestParam("number") Integer number,
                         @NotNull(message = "size不能为空") @RequestParam("size") Integer size,
                         @RequestParam(value = "ascend", required = false) Boolean ascend,
                         @RequestParam(value = "reviewRequired", required = false) Boolean reviewRequired) {
        //        int uid = currentUser.getCurrentUser().getUserId();
        int uid = REVIEWER_ID;

        if (ascend == null) {
            ascend = true;
        }

        Page<Post> page;
        if(reviewRequired==null){
            page = postService.getAllByRev(uid, number, size, ascend);
        }else{
            page = postService.getAllRequiredToReview(uid,reviewRequired,number,size,ascend);
        }



        return AjaxResponse.success(DozerUtil.mapPage(page, Post4RevSimpleVO.class));
    }

    @ApiOperation(value = "根据id删除期刊记录")
    @DeleteMapping("/{id}")
    @PreAuthorize("hasAnyRole('ROLE_ADMIN')")
    public AjaxResponse deletePost(@NotNull(message = "id不能为空") @PathVariable Integer id) {

        postService.deletePost(id);

        return AjaxResponse.success();
    }

    private void checkPostCanSubmit(Post post) {
        // 检查标题信息
        String title = post.getTitle();
        if (title == null || title.isBlank()) {
            throw new CustomException(CustomExceptionType.USER_INPUT_ERROR, ErrMsg.TITLE_NEEDED);
        }

        // 检查投稿领域信息
        if (post.getField() == null) {
            throw new CustomException(CustomExceptionType.USER_INPUT_ERROR, ErrMsg.FIELD_NEEDED);
        }

        // 检查体裁信息
        Integer genre = post.getGenre();
        if (genre == null) {
            throw new CustomException(CustomExceptionType.USER_INPUT_ERROR, ErrMsg.GENRE_NEEDED);
        }

        // 检查作者信息
        String writersInfo = post.getWritersInfo();
        if (writersInfo == null || writersInfo.isBlank()) {
            throw new CustomException(CustomExceptionType.USER_INPUT_ERROR, ErrMsg.WRITERS_INFO_NEEDED);
        }

        // 检查基金级别
        Integer fundLevel = post.getFundLevel();
        if (fundLevel == null) {
            throw new CustomException(CustomExceptionType.USER_INPUT_ERROR, ErrMsg.FUND_LEVEL_NEEDED);
        }

        // 检查文件上传情况
        String fundApprovalPath = post.getFundApprovalPath();
        if (post.getPostPath() == null || post.getLetterPath() == null || post.getEthicsApprovalPath() == null) {
            throw new CustomException(CustomExceptionType.USER_INPUT_ERROR, ErrMsg.INCOMPLETE_POST_FILES);
        }
        if (fundLevel != FundLevel.NO.getCode() && fundApprovalPath == null) {
            throw new CustomException(CustomExceptionType.USER_INPUT_ERROR, ErrMsg.INCOMPLETE_POST_FILES);
        }

        // 检查关键字和摘要填写情况
        String abstractZh = post.getAbstractZh();
        String abstractEn = post.getAbstractEn();
        String keywordsZh = post.getKeywordsZh();
        String keywordsEn = post.getAbstractEn();
        boolean hasZhKw = keywordsZh != null && !keywordsZh.isBlank();
        boolean hasEnKw = keywordsEn != null && !keywordsEn.isBlank();
        boolean hasZhAbstract = abstractZh != null && !abstractZh.isBlank();
        boolean hasEnAbstract = abstractEn != null && !abstractEn.isBlank();

        if (genre == WORKS.getCode()) {
            if (!(hasZhKw && hasEnKw && hasZhAbstract && hasEnAbstract)) {
                throw new CustomException(CustomExceptionType.USER_INPUT_ERROR, ErrMsg.INCOMPLETE_KEYWORDS_AND_ABSTRACTS);
            }
        } else if (genre == OVERVIEW.getCode()) {
            if (!(hasZhKw && hasZhAbstract)) {
                throw new CustomException(CustomExceptionType.USER_INPUT_ERROR, ErrMsg.INCOMPLETE_KEYWORDS_AND_ABSTRACTS);
            }
        } else if (genre == PAPER.getCode()) {
            if (!(hasZhKw && hasZhAbstract)) {
                throw new CustomException(CustomExceptionType.USER_INPUT_ERROR, ErrMsg.INCOMPLETE_KEYWORDS_AND_ABSTRACTS);
            }
        } else if (Genre.getItem(genre) == null) {
            throw new CustomException(CustomExceptionType.USER_INPUT_ERROR, ErrMsg.UNSUPPORTED_GENRE);
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
            if (taxpayerId == null || taxpayerId.isBlank() ||
                    address == null || address.isBlank() ||
                    receiver == null || receiver.isBlank() ||
                    invoiceTitle == null || invoiceTitle.isBlank()) {
                throw new CustomException(CustomExceptionType.USER_INPUT_ERROR, ErrMsg.INCOMPLETE_INVOICE_INFO);
            }
        }
    }
}
