package com.samsph.pjcm.dao;

import com.samsph.pjcm.model.User;
import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.data.jpa.repository.JpaSpecificationExecutor;

import java.util.Optional;


public interface UserRepository extends JpaRepository<User,Integer>, JpaSpecificationExecutor<User> {
    /**
     * 根据邮箱得到用户
     * @param email 邮箱
     * @return User
     */
    Optional<User> findByEmail(String email);

    /**
     * 根据code得到用户
     * @param code 激活码
     * @return User
     */
    Optional<User> findByCode(String code);
}
