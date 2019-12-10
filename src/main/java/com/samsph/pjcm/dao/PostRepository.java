package com.samsph.pjcm.dao;

import com.samsph.pjcm.model.Post;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.Pageable;
import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.data.jpa.repository.Query;

import java.util.List;

/**
 * 投稿表数据操作接口
 *
 * @author hujiahao
 */
public interface PostRepository extends JpaRepository<Post, Integer> {


    /**
     * 获取某一期刊包含的投稿列表
     *
     * @param jid 期刊id
     * @return List<Post>
     */
    List<Post> findByJid(int jid);

    /**
     * 以分页的方式查询所有投稿
     *
     * @param page 分页请求
     * @return Page<Post>
     */
    @Override
    @Query(value = "SELECT * FROM post",
            countQuery = "SELECT count(*) FROM post",
            nativeQuery = true)
    Page<Post> findAll(Pageable page);

    /**
     * 以分页的方式查询所有投稿
     *
     * @param page   分页请求
     * @param status 投稿状态
     * @return Page<Post>
     */
    @Query(value = "SELECT * FROM post p WHERE p.status=?1",
            countQuery = "SELECT count(*) FROM post WHERE p.status=?1",
            nativeQuery = true)
    Page<Post> findAllByStatus(int status, Pageable page);
}
