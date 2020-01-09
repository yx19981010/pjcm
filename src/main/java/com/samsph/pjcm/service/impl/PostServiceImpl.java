package com.samsph.pjcm.service.impl;

import com.samsph.pjcm.config.constant.*;
import com.samsph.pjcm.config.exception.CustomException;
import com.samsph.pjcm.config.exception.CustomExceptionType;
import com.samsph.pjcm.dao.PostRepository;
import com.samsph.pjcm.model.Post;
import com.samsph.pjcm.service.PostService;
import com.samsph.pjcm.query.PostQuery;
import org.dozer.Mapper;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.PageRequest;
import org.springframework.data.domain.Sort;
import org.springframework.stereotype.Service;

import javax.annotation.Resource;
import java.util.Date;
import java.util.List;
import java.util.Optional;

import static com.samsph.pjcm.config.constant.PostStatus.TO_BE_SUBMITTED;

/**
 * @author hujiahao
 */
@Service
public class PostServiceImpl implements PostService {
    @Resource
    private PostRepository postRepository;

    @Resource
    private Mapper dozerMapper;

    @Override
    public Post savePost(PostQuery postQuery, int creatorId) {
        Post post = dozerMapper.map(postQuery, Post.class);

        // 设置稿件创建时间、审阅轮数
        post.setCreateTime(new Date());
        post.setCount(0);
        post.setInvoiceNeeded(MyBoolean.DEFAULT.getCode());

        // 设置稿件初始状态
        post.setStatus(TO_BE_SUBMITTED.getCode());

        // 设置稿件投稿人id
        post.setContributorUid(creatorId);

        return postRepository.save(post);
    }

    @Override
    public Post getPost(int id) {
        return fetchPost(id);
    }

    @Override
    public void updatePost(Post post) {
        postRepository.save(post);
    }

    @Override
    public Page<Post> getAllByCtrUid(int uid, int number, int size, boolean ascend) {
        // 新建分页选项
        Sort.Direction direction = ascend ? Sort.Direction.ASC : Sort.Direction.DESC;
        PageRequest pageRequest = PageRequest.of(number - 1, size, direction, "id");

        // 查询分页结果
        return postRepository.findByContributorUid(uid, pageRequest);
    }

    @Override
    public Page<Post> getAllByCtrUidAndSubmitTime(int uid, Date start, Date end, int number, int size, boolean ascend) {
        // 新建分页选项
        Sort.Direction direction = ascend ? Sort.Direction.ASC : Sort.Direction.DESC;
        PageRequest pageRequest = PageRequest.of(number - 1, size, direction, "id");

        // 查询分页结果
        return postRepository.findByContributorUidAndSubmitTimeAfterAndSubmitTimeBefore(uid, start, end, pageRequest);
    }

    @Override
    public Page<Post> getAllByCtrUidAndStatus(int uid, List<Integer> statuses, int number, int size, boolean ascend) {
        // 新建分页选项
        Sort.Direction direction = ascend ? Sort.Direction.ASC : Sort.Direction.DESC;
        PageRequest pageRequest = PageRequest.of(number - 1, size, direction, "id");

        // 查询分页结果
        return postRepository.findByContributorUidAndStatusIn(uid, statuses, pageRequest);
    }

    @Override
    public Page<Post> getAllByCtrUidAndStatusAndSubmitTime(int uid, List<Integer> statuses, Date start, Date end, int number, int size, boolean ascend) {
        // 新建分页选项
        Sort.Direction direction = ascend ? Sort.Direction.ASC : Sort.Direction.DESC;
        PageRequest pageRequest = PageRequest.of(number - 1, size, direction, "id");

        // 查询分页结果
        return postRepository.findByContributorUidAndStatusInAndSubmitTimeAfterAndSubmitTimeBefore(uid, statuses, start, end, pageRequest);
    }


    @Override
    public Page<Post> getAllByEdUid(int uid, int number, int size, boolean ascend) {
        // 新建分页选项
        Sort.Direction direction = ascend ? Sort.Direction.ASC : Sort.Direction.DESC;
        PageRequest pageRequest = PageRequest.of(number - 1, size, direction, "id");

        // 查询分页结果
        return postRepository.findByEditorUid(uid, pageRequest);
    }

    @Override
    public Page<Post> getAllByEdUidAndSubmitTime(int uid, Date start, Date end, int number, int size, boolean ascend) {
        // 新建分页选项
        Sort.Direction direction = ascend ? Sort.Direction.ASC : Sort.Direction.DESC;
        PageRequest pageRequest = PageRequest.of(number - 1, size, direction, "id");

        // 查询分页结果
        return postRepository.findByEditorUidAndSubmitTimeAfterAndSubmitTimeBefore(uid, start, end, pageRequest);
    }


    @Override
    public Page<Post> getAllByEdUidAndStatus(int uid, List<Integer> statuses, int number, int size, boolean ascend) {
        // 新建分页选项
        Sort.Direction direction = ascend ? Sort.Direction.ASC : Sort.Direction.DESC;
        PageRequest pageRequest = PageRequest.of(number - 1, size, direction, "id");

        // 查询分页结果
        return postRepository.findByEditorUidAndStatusIn(uid, statuses, pageRequest);
    }

    @Override
    public Page<Post> getAllByEdUidAndStatusAndSubmitTime(int uid, List<Integer> statuses, Date start, Date end, int number, int size, boolean ascend) {
        // 新建分页选项
        Sort.Direction direction = ascend ? Sort.Direction.ASC : Sort.Direction.DESC;
        PageRequest pageRequest = PageRequest.of(number - 1, size, direction, "id");

        // 查询分页结果
        return postRepository.findByEditorUidAndStatusInAndSubmitTimeAfterAndSubmitTimeBefore(uid, statuses, start, end, pageRequest);
    }

    @Override
    public Page<Post> getAllByRevUidAndAccept(int uid, MyBoolean accept, int number, int size, boolean ascend) {
        // 新建分页选项
        Sort.Direction direction = ascend ? Sort.Direction.ASC : Sort.Direction.DESC;
        PageRequest pageRequest = PageRequest.of(number - 1, size, direction, "id");

        // 查询分页结果
        return postRepository.findByReviewerUidAndAccept(uid, accept.getCode(), pageRequest);
    }

    @Override
    public Page<Post> getAllByRevUidAndAcceptAndSubmitTime(int uid, MyBoolean accept, Date start, Date end, int number, int size, boolean ascend) {
        // 新建分页选项
        Sort.Direction direction = ascend ? Sort.Direction.ASC : Sort.Direction.DESC;
        PageRequest pageRequest = PageRequest.of(number - 1, size, direction, "id");

        // 查询分页结果
        return postRepository.findByReviewerUidAndAcceptAndSubmitTimeAfterAndSubmitTimeBefore(uid, accept.getCode(), start, end, pageRequest);
    }

    @Override
    public Page<Post> getAllByRevUidAndFlag(int uid, boolean flag, int number, int size, boolean ascend) {
        // 新建分页选项
        Sort.Direction direction = ascend ? Sort.Direction.ASC : Sort.Direction.DESC;
        PageRequest pageRequest = PageRequest.of(number - 1, size, direction, "id");

        // 查询分页结果
        if(flag){
            return postRepository.findByReviewerUidAndFlagTrue(uid, pageRequest);
        }else{
            return postRepository.findByReviewerUidAndFlagFalse(uid, pageRequest);
        }
    }

    @Override
    public Page<Post> getAllByRevUidAndFlagAndSubmitTime(int uid, boolean flag, Date start, Date end, int number, int size, boolean ascend) {
        // 新建分页选项
        Sort.Direction direction = ascend ? Sort.Direction.ASC : Sort.Direction.DESC;
        PageRequest pageRequest = PageRequest.of(number - 1, size, direction, "id");

        // 查询分页结果
        return postRepository.findByReviewerUidAndFlagAndSubmitTimeAfterAndSubmitTimeBefore(uid, flag ? MyBoolean.TRUE.getCode() : MyBoolean.FALSE.getCode(), start, end, pageRequest);
    }

    @Override
    public void deletePost(int id) {
        fetchPost(id);

        // 删除投稿
        postRepository.deleteById(id);
    }


    private Post fetchPost(int id) {
        // 查询是否存在该id的投稿
        Optional<Post> postOptional = postRepository.findById(id);

        // 若不存在则抛出异常
        if (!postOptional.isPresent()) {
            throw new CustomException(CustomExceptionType.USER_INPUT_ERROR, ErrMsg.POST_NOT_FOUND);
        }

        return postOptional.get();
    }

}
