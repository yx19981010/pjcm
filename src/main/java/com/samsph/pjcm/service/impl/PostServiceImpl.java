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
    public Page<Post> getAllByJid(int jid, Integer number, Integer size, Boolean ascend) {

        // 新建分页选项
        Sort.Direction direction = ascend ? Sort.Direction.ASC : Sort.Direction.DESC;
        PageRequest pageRequest = PageRequest.of(number - 1, size, direction, "id");

        // 查询分页结果
        return postRepository.findAllByJid(jid, pageRequest);
    }

    @Override
    public Page<Post> getAllByCtr(int uid, int number, int size, boolean ascend) {
        // 新建分页选项
        Sort.Direction direction = ascend ? Sort.Direction.ASC : Sort.Direction.DESC;
        PageRequest pageRequest = PageRequest.of(number - 1, size, direction, "id");

        // 查询分页结果
        return postRepository.findAllByCtr(uid, pageRequest);
    }

    @Override
    public Page<Post> getAllByCtrAndStatus(int uid, PostStatus status, int number, int size, boolean ascend) {
        // 新建分页选项
        Sort.Direction direction = ascend ? Sort.Direction.ASC : Sort.Direction.DESC;
        PageRequest pageRequest = PageRequest.of(number - 1, size, direction, "id");

        // 查询分页结果
        return postRepository.findAllByCtrAndStatus(uid, status.getCode(), pageRequest);
    }

    @Override
    public Page<Post> getAllByEd(int uid, Integer number, Integer size, Boolean ascend) {
        // 新建分页选项
        Sort.Direction direction = ascend ? Sort.Direction.ASC : Sort.Direction.DESC;
        PageRequest pageRequest = PageRequest.of(number - 1, size, direction, "id");

        // 查询分页结果
        return postRepository.findAllByEd(uid, pageRequest);
    }

    @Override
    public Page<Post> getAllByEdAndStatus(int uid, PostStatus status, Integer number, Integer size, Boolean ascend) {
        // 新建分页选项
        Sort.Direction direction = ascend ? Sort.Direction.ASC : Sort.Direction.DESC;
        PageRequest pageRequest = PageRequest.of(number - 1, size, direction, "id");

        // 查询分页结果
        return postRepository.findAllByEdAndStatus(uid, status.getCode(), pageRequest);
    }

    @Override
    public Page<Post> getAllByRevUnanswer(int uid, Integer number, Integer size, Boolean ascend) {
        // 新建分页选项
        Sort.Direction direction = ascend ? Sort.Direction.ASC : Sort.Direction.DESC;
        PageRequest pageRequest = PageRequest.of(number - 1, size, direction, "id");

        // 查询分页结果
        return postRepository.findAllByRevUidAndAccept(uid, MyBoolean.DEFAULT.getCode(), pageRequest);
    }

    @Override
    public Page<Post> getAllByRev(int uid, Integer number, Integer size, Boolean ascend) {
        // 新建分页选项
        Sort.Direction direction = ascend ? Sort.Direction.ASC : Sort.Direction.DESC;
        PageRequest pageRequest = PageRequest.of(number - 1, size, direction, "id");

        // 查询分页结果
        return postRepository.findAllByRev(uid, pageRequest);
    }

    @Override
    public Page<Post> getAllRequiredToReview(int uid, boolean reviewRequired, Integer number, Integer size, Boolean ascend) {
        // 新建分页选项
        Sort.Direction direction = ascend ? Sort.Direction.ASC : Sort.Direction.DESC;
        PageRequest pageRequest = PageRequest.of(number - 1, size, direction, "id");

        // 查询分页结果
        return postRepository.findAllRequiredToReview(uid,
                reviewRequired ? MyBoolean.TRUE.getCode() : MyBoolean.FALSE.getCode(),
                pageRequest);
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
        if (postOptional.isEmpty()) {
            throw new CustomException(CustomExceptionType.USER_INPUT_ERROR, ErrMsg.POST_NOT_FOUND);
        }

        return postOptional.get();
    }

}
