package com.samsph.pjcm.service;

import com.samsph.pjcm.model.User;
import com.samsph.pjcm.vo.UserERVoPost;
import com.samsph.pjcm.vo.UserVoPost;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.Pageable;


import java.util.Optional;

public interface UserService {
     void addUser(UserERVoPost userERVoPost);

     void updateUser(User user);

     void deleteUser(int id);

     Optional<User> findUserByUid(int uid);

     Optional<User> findUserByEmail(String email);

     Page<User> findUsersByRid(int rid, Pageable pageable);

     Optional<User> findUserByCode(String code);

     void saveUser(UserVoPost userVoPost);

}
