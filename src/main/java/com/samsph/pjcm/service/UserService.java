package com.samsph.pjcm.service;

import com.samsph.pjcm.model.User;
import com.samsph.pjcm.vo.UserERVoPost;
import com.samsph.pjcm.vo.UserVoPost;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.Pageable;


import java.util.Optional;

public interface UserService {
     /**
      * 添加系统用户
      * @param userERVoPost 系统用户实体
      */
     void addUser(UserERVoPost userERVoPost);

     /**
      * 更新用户
      * @param user 用户实体
      */
     void updateUser(User user);

     /**
      *删除用户
      * @param id 用户id
      */
     void deleteUser(int id);

     /**
      *根据用户id得到用户
      * @param uid 用户id
      * @return 用户
      */
     Optional<User> findUserByUid(int uid);

     /**
      *根据邮箱得到用户
      * @param email 邮箱
      * @return 用户
      */
     Optional<User> findUserByEmail(String email);

     /**
      *根据角色id得到分页的用户列表
      * @param rid 角色id
      * @param pageable 分页参数
      * @return 分页的用户列表
      */
     Page<User> findUsersByRid(int rid, Pageable pageable);

     /**
      *根据code得到用户
      * @param code 激活码
      * @return 用户
      */
     Optional<User> findUserByCode(String code);

     /**
      *添加投稿人
      * @param userVoPost 投稿人实体
      */
     void saveUser(UserVoPost userVoPost);
}
