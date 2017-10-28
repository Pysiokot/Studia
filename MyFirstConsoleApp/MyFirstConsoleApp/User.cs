using System;
using System.Collections.Generic;
using System.ComponentModel.DataAnnotations;
using System.Linq;
using System.Text;
using System.Threading.Tasks;

namespace MyFirstConsoleApp
{
    class User
    {
        [Key]
        public string UserName { get; set; }
        public string Description { get; set; }
    }
}
