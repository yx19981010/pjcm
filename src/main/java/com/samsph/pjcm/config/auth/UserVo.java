package com.samsph.pjcm.config.auth;

import lombok.AllArgsConstructor;
import lombok.Data;
import lombok.NoArgsConstructor;

@Data
@NoArgsConstructor
@AllArgsConstructor
public class UserVo {
    int userId;
    int userRole;
    String userName;
    String time;
}
